#' Background job registry for franzpak
#'
#' These helpers wrap [callr::r_bg()] to execute potentially long-running model
#' fits in isolated background R sessions. Jobs are tracked in an in-memory
#' registry so that callers can poll for completion, collect results, and read
#' log output.
#'
#' @keywords internal

# Internal registry for handles -------------------------------------------------
.bgjm_env <- new.env(parent = emptyenv())
.bgjm_env$jobs <- list()

#' Generate background job identifiers
#'
#' @param prefix Character prefix prepended to the identifier.
#' @return A length-one character vector containing the identifier.
#' @keywords internal
.bgjm_now_id <- function(prefix = "job") {
  paste0(
    prefix,
    "-",
    format(Sys.time(), "%Y%m%d-%H%M%S"),
    "-",
    substr(as.hexmode(sample.int(.Machine$integer.max, 1)), 1, 6)
  )
}

#' Generate a unique job ID based on user-provided name or auto-generated ID
#'
#' If `job_name` is provided, tries to use it as-is. If that name already exists
#' in the registry, appends -1, -2, etc. until a unique name is found.
#' If `job_name` is NULL, generates a timestamped ID.
#'
#' @param job_name Optional user-provided job name.
#' @return A unique job identifier string.
#' @keywords internal
.bgjm_make_unique_id <- function(job_name = NULL) {
  if (is.null(job_name)) {
    # No name provided - use timestamp approach
    return(.bgjm_now_id("job"))
  }

  # User provided a name - try to use it as-is first
  if (!job_name %in% names(.bgjm_env$jobs)) {
    return(job_name)
  }

  # Name already exists - find next available number
  i <- 1
  while (TRUE) {
    candidate <- paste0(job_name, "-", i)
    if (!candidate %in% names(.bgjm_env$jobs)) {
      return(candidate)
    }
    i <- i + 1
  }
}

#' Return the left operand if not `NULL`
#'
#' This is a lightweight infix helper used to provide default values.
#'
#' @param x Left-hand side.
#' @param y Right-hand side.
#' @return `x` when it is not `NULL`, otherwise `y`.
#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Launch a background job
#'
#' Utility used by the higher-level wrappers to start work in a separate
#' R session via [callr::r_bg()]. The job is registered in a package-local
#' environment so that it can later be polled or collected.
#'
#' @param fun_expr A quoted function definition to be evaluated in the child
#'   process. This must return a function object.
#' @param args Named list with arguments forwarded to the target function.
#' @param job_name Optional friendly name for the job; defaults to the job id.
#' @param output_base Directory where per-job folders will be created.
#' @param packages Character vector of packages to load in the child process.
#' @param libpath Library paths passed to [callr::r_bg()].
#' @param env Named character vector of environment variables for the child
#'   (defaults to [callr::rcmd_safe_env()]).
#' @param seed Optional integer seed forwarded to [set.seed()] inside the job.
#' @param return_strategy Either `"inline"` to return results through IPC or
#'   `"rds"` to persist the result as an `.rds` file.
#'
#' @return Invisible job identifier string.
#' @keywords internal
#' @importFrom callr r_bg rcmd_safe_env poll
.bgjm_start_job <- function(fun_expr,
                            args = list(),
                            job_name = NULL,
                            output_base = tempdir(),
                            packages = character(),
                            libpath = .libPaths(),
                            env = rcmd_safe_env(),
                            seed = NULL,
                            return_strategy = c("inline", "rds")) {
  return_strategy <- match.arg(return_strategy)

  job_id <- .bgjm_make_unique_id(job_name)
  job_dir <- file.path(normalizePath(output_base, mustWork = FALSE), job_id)
  dir.create(job_dir, recursive = TRUE, showWarnings = FALSE)
  tmp_dir <- file.path(job_dir, "tmp")
  art_dir <- file.path(job_dir, "artifacts")
  dir.create(tmp_dir, showWarnings = FALSE)
  dir.create(art_dir, showWarnings = FALSE)

  stdout_log <- file.path(job_dir, "stdout.log")
  stderr_log <- file.path(job_dir, "stderr.log")

  child <- function(.fun_expr,
                    .args,
                    .job_dir,
                    .tmp_dir,
                    .art_dir,
                    .packages,
                    .seed,
                    .return_strategy) {
    if (length(.packages)) {
      for (p in .packages) {
        suppressPackageStartupMessages(require(p, character.only = TRUE))
      }
    }

    old_wd <- setwd(.job_dir)
    on.exit(setwd(old_wd), add = TRUE)
    Sys.setenv(TMPDIR = .tmp_dir)

    if (!is.null(.seed)) {
      set.seed(.seed)
    }

    before <- character(0)
    if (dir.exists(.job_dir)) {
      before <- list.files(
        .job_dir,
        all.files = TRUE,
        recursive = TRUE,
        include.dirs = FALSE
      )
    }

    target_fun <- eval(.fun_expr, envir = baseenv())
    res <- try(do.call(target_fun, .args), silent = TRUE)

    after <- list.files(
      .job_dir,
      all.files = TRUE,
      recursive = TRUE,
      include.dirs = FALSE
    )

    new_rel <- setdiff(after, before)
    new_rel <- new_rel[!grepl("^tmp(/|$)|^artifacts(/|$)", new_rel)]

    moved <- character(0)
    for (rel in new_rel) {
      src <- file.path(.job_dir, rel)
      if (file.exists(src) && !dir.exists(src)) {
        dst <- file.path(.art_dir, basename(rel))
        ok <- tryCatch(
          file.rename(src, dst),
          warning = function(w) FALSE,
          error = function(e) FALSE
        )
        if (!ok) {
          ok2 <- tryCatch(
            file.copy(src, dst, overwrite = TRUE),
            warning = function(w) FALSE,
            error = function(e) FALSE
          )
          if (ok2) {
            unlink(src)
          }
        }
        if (file.exists(file.path(.art_dir, basename(rel)))) {
          moved <- c(moved, file.path(.art_dir, basename(rel)))
        }
      }
    }

    res_path <- NULL
    if (identical(.return_strategy, "rds")) {
      res_path <- file.path(.job_dir, "result.rds")
      saveRDS(res, res_path)
      res <- NULL
    }

    list(
      ok = !inherits(res, "try-error"),
      result = res,
      result_rds = res_path,
      job_dir = .job_dir,
      artifacts_dir = .art_dir,
      artifact_files = moved
    )
  }

  handle <- r_bg(
    func = child,
    args = list(
      .fun_expr = fun_expr,
      .args = args,
      .job_dir = job_dir,
      .tmp_dir = tmp_dir,
      .art_dir = art_dir,
      .packages = packages,
      .seed = seed,
      .return_strategy = return_strategy
    ),
    libpath = libpath,
    stdout = stdout_log,
    stderr = stderr_log,
    supervise = TRUE,
    system_profile = FALSE,
    user_profile = FALSE,
    env = env
  )

  .bgjm_env$jobs[[job_id]] <- list(
    id = job_id,
    name = job_name %||% job_id,
    dir = job_dir,
    stdout = stdout_log,
    stderr = stderr_log,
    handle = handle,
    started = Sys.time()
  )

  invisible(job_id)
}

#' Start a tidyLPA background job
#'
#' Creates a background session that evaluates [tidyLPA::estimate_profiles()]
#' with the supplied data and configuration.
#'
#' @param data A data frame passed to `estimate_profiles()`.
#' @param n_profiles Number of profiles to estimate.
#' @param ... Additional arguments forwarded to `estimate_profiles()`, such as
#'   `models`, `variances`, `covariances`, etc. See tidyLPA documentation for details.
#' @param package `"mclust"` or `"Mplus"` backend indicator (tidyLPA >= 1.0 naming).
#' @param job_name Optional friendly job name.
#' @param output_base Directory where job folders should be created.
#' @param seed Optional random seed for reproducibility.
#' @param return_strategy Return result inline or as serialized RDS.
#' @param pass_data_as Whether to pass the data object or a temporary RDS path.
#' @param packages Additional packages to load in the child session.
#'
#' @details When `package = "Mplus"` you must have Mplus and
#'   `MplusAutomation` available. Any files written by the child session are
#'   moved to the job's `artifacts` directory.
#'
#'   This function uses the tidyLPA >= 1.0 API. By default, models are constructed
#'   from `variances` and `covariances` arguments (passed via `...`). You can also
#'   explicitly specify `models` (e.g., `models = 1` or `models = c(1, 2, 3)`) via `...`.
#'
#' @return Invisible job identifier string.
#' @export
#' @examplesIf interactive()
#' \dontrun{
#' # Using variances and covariances to specify model
#' job_id <- bgjm_start_tidylpa(
#'   data = iris[, 1:4],
#'   n_profiles = 3,
#'   variances = c("equal", "equal"),
#'   covariances = c("zero", "zero"),
#'   package = "mclust",
#'   job_name = "iris-demo"
#' )
#'
#' bgjm_poll(0)
#' }
bgjm_start_tidylpa <- function(data,
                               n_profiles,
                               ...,
                               package = c("mclust", "Mplus"),
                               job_name = NULL,
                               output_base = tempdir(),
                               seed = NULL,
                               return_strategy = c("inline", "rds"),
                               pass_data_as = c("object", "rds"),
                               packages = c("tidyLPA")) {
  package <- match.arg(package)
  return_strategy <- match.arg(return_strategy)
  pass_data_as <- match.arg(pass_data_as)

  data_arg <- switch(
    pass_data_as,
    object = data,
    rds = {
      dir.create(output_base, showWarnings = FALSE, recursive = TRUE)
      tmp_path <- file.path(
        output_base,
        paste0("bgjm-data-", .bgjm_now_id("tidylpa"), ".rds")
      )
      saveRDS(data, tmp_path)
      tmp_path
    }
  )

  fun_expr <- if (pass_data_as == "object") {
    substitute(
      function(.data, .n_profiles, .package, ...) {
        tidyLPA::estimate_profiles(
          .data,
          n_profiles = .n_profiles,
          package = .package,
          ...
        )
      }
    )
  } else {
    substitute(
      function(.data_rds_path, .n_profiles, .package, ...) {
        .data <- readRDS(.data_rds_path)
        tidyLPA::estimate_profiles(
          .data,
          n_profiles = .n_profiles,
          package = .package,
          ...
        )
      }
    )
  }

  args <- if (pass_data_as == "object") {
    list(.data = data_arg, .n_profiles = n_profiles, .package = package, ...)
  } else {
    list(.data_rds_path = data_arg, .n_profiles = n_profiles, .package = package, ...)
  }

  .bgjm_start_job(
    fun_expr = fun_expr,
    args = args,
    job_name = job_name %||% sprintf("tidylpa-n%d-%s", n_profiles, package),
    output_base = output_base,
    packages = unique(c(packages, if (package == "Mplus") "MplusAutomation")),
    seed = seed,
    return_strategy = return_strategy
  )
}

#' Start a lavaan background job
#'
#' Launches a child process that runs one of `lavaan::sem()`, `lavaan::cfa()`,
#' or `lavaan::lavaan()` with the provided syntax and data.
#'
#' @param model_syntax Character vector containing lavaan model syntax.
#' @param data Data frame passed to the lavaan function.
#' @param lavaan_fun One of `"sem"`, `"cfa"`, or `"lavaan"`.
#' @param ... Additional arguments forwarded to the selected lavaan function.
#' @param job_name Optional friendly job name.
#' @param output_base Directory where job folders should be created.
#' @param seed Optional random seed.
#' @param return_strategy Return result inline or as serialized RDS.
#' @param pass_data_as Whether data are passed as an object or temporary RDS.
#' @param packages Additional packages to load in the child session.
#'
#' @return Invisible job identifier string.
#' @export
#' @examplesIf interactive()
#' \dontrun{
#' job_id <- bgjm_start_lavaan(
#'   model_syntax = "f1 =~ x1 + x2 + x3",
#'   data = HolzingerSwineford1939,
#'   lavaan_fun = "sem"
#' )
#' }
bgjm_start_lavaan <- function(model_syntax,
                              data,
                              lavaan_fun = c("sem", "cfa", "lavaan"),
                              ...,
                              job_name = NULL,
                              output_base = tempdir(),
                              seed = NULL,
                              return_strategy = c("inline", "rds"),
                              pass_data_as = c("object", "rds"),
                              packages = c("lavaan")) {
  lavaan_fun <- match.arg(lavaan_fun)
  return_strategy <- match.arg(return_strategy)
  pass_data_as <- match.arg(pass_data_as)

  data_arg <- switch(
    pass_data_as,
    object = data,
    rds = {
      dir.create(output_base, showWarnings = FALSE, recursive = TRUE)
      tmp_path <- file.path(
        output_base,
        paste0("bgjm-data-", .bgjm_now_id("lavaan"), ".rds")
      )
      saveRDS(data, tmp_path)
      tmp_path
    }
  )

  fun_expr <- if (pass_data_as == "object") {
    substitute(
      function(.syntax, .data, .fn, ...) {
        f <- get(.fn, asNamespace("lavaan"))
        f(.syntax, data = .data, ...)
      }
    )
  } else {
    substitute(
      function(.syntax, .data_rds_path, .fn, ...) {
        .data <- readRDS(.data_rds_path)
        f <- get(.fn, asNamespace("lavaan"))
        f(.syntax, data = .data, ...)
      }
    )
  }

  args <- if (pass_data_as == "object") {
    list(.syntax = model_syntax, .data = data_arg, .fn = lavaan_fun, ...)
  } else {
    list(.syntax = model_syntax, .data_rds_path = data_arg, .fn = lavaan_fun, ...)
  }

  .bgjm_start_job(
    fun_expr = fun_expr,
    args = args,
    job_name = job_name %||% sprintf("lavaan-%s", lavaan_fun),
    output_base = output_base,
    packages = packages,
    seed = seed,
    return_strategy = return_strategy
  )
}

#' List registered background jobs
#'
#' @return A data frame describing tracked jobs; an empty data frame when none
#'   are registered.
#' @export
bgjm_list <- function() {
  if (!length(.bgjm_env$jobs)) {
    return(data.frame())
  }
  info <- lapply(.bgjm_env$jobs, function(j) {
    h <- j$handle
    status <- if (!h$is_alive()) {
      if (h$get_exit_status() == 0) "finished" else "error"
    } else {
      "running"
    }
    data.frame(
      id = j$id,
      name = j$name,
      status = status,
      started = j$started,
      dir = j$dir,
      stdout = j$stdout,
      stderr = j$stderr,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, info)
}

#' Query job status
#'
#' @param job_id Identifier returned by a `bgjm_start_*()` function.
#'
#' @return `"running"`, `"finished"`, or `"error"`.
#' @export
bgjm_status <- function(job_id) {
  j <- .bgjm_env$jobs[[job_id]]
  if (is.null(j)) {
    stop("Unknown job_id: ", job_id)
  }
  h <- j$handle
  if (h$is_alive()) {
    return("running")
  }
  if (h$get_exit_status() == 0) {
    "finished"
  } else {
    "error"
  }
}

#' Poll background jobs
#'
#' @param timeout_ms Milliseconds to wait inside [callr::poll()].
#'
#' @return Invisible poll result from [callr::poll()].
#' @export
bgjm_poll <- function(timeout_ms = 0) {
  if (!length(.bgjm_env$jobs)) {
    return(invisible(NULL))
  }
  procs <- lapply(.bgjm_env$jobs, `[[`, "handle")
  names(procs) <- names(.bgjm_env$jobs)
  poll(procs, ms = timeout_ms)
}

#' Collect the result of a background job
#'
#' @param job_id Identifier returned by a `bgjm_start_*()` function.
#' @param read_rds When `TRUE`, read serialized results produced with the `"rds"`
#'   strategy.
#' @param auto_remove If `TRUE`, remove the job from the registry after
#'   collection.
#'
#' @return A list containing the job metadata, logs, and result object.
#' @export
bgjm_collect <- function(job_id, read_rds = TRUE, auto_remove = FALSE) {
  j <- .bgjm_env$jobs[[job_id]]
  if (is.null(j)) {
    stop("Unknown job_id: ", job_id)
  }
  h <- j$handle
  if (h$is_alive()) {
    stop("Job still running: ", job_id)
  }

  if (h$get_exit_status() != 0) {
    msg <- tryCatch({
      rr <- h$get_result()
      if (is.list(rr) && isFALSE(rr$ok)) {
        "Child reported error; inspect logs."
      } else {
        "Child process exited with error; inspect logs."
      }
    }, error = function(e) {
      "Child process failed before returning a result; inspect logs."
    })
    stop(paste(
      msg,
      "\nstdout:", j$stdout,
      "\nstderr:", j$stderr
    ))
  }

  rr <- h$get_result()
  result <- rr$result
  if (is.null(result) && read_rds && !is.null(rr$result_rds) && file.exists(rr$result_rds)) {
    result <- readRDS(rr$result_rds)
  }

  out <- list(
    id = job_id,
    name = j$name,
    result = result,
    result_rds = rr$result_rds,
    artifacts_dir = rr$artifacts_dir,
    artifact_files = rr$artifact_files,
    job_dir = rr$job_dir,
    stdout = j$stdout,
    stderr = j$stderr
  )

  if (isTRUE(auto_remove)) {
    bgjm_remove(job_id, delete_dir = TRUE)
  }

  out
}

#' Terminate a running job
#'
#' @param job_id Identifier returned by a `bgjm_start_*()` function.
#'
#' @return Invisible `TRUE` when the signal is sent successfully.
#' @export
bgjm_kill <- function(job_id) {
  j <- .bgjm_env$jobs[[job_id]]
  if (is.null(j)) {
    stop("Unknown job_id: ", job_id)
  }
  j$handle$kill()
  invisible(TRUE)
}

#' Remove a job from the registry
#'
#' @param job_id Identifier returned by a `bgjm_start_*()` function.
#' @param delete_dir When `TRUE`, delete the job's working directory.
#'
#' @return Invisible logical indicating whether the job existed.
#' @export
bgjm_remove <- function(job_id, delete_dir = FALSE) {
  j <- .bgjm_env$jobs[[job_id]]
  if (is.null(j)) {
    return(invisible(FALSE))
  }
  if (j$handle$is_alive()) {
    stop("Cannot remove a running job.")
  }
  if (isTRUE(delete_dir) && dir.exists(j$dir)) {
    unlink(j$dir, recursive = TRUE, force = TRUE)
  }
  .bgjm_env$jobs[[job_id]] <- NULL
  invisible(TRUE)
}

#' Read the captured standard output
#'
#' @param job_id Identifier returned by a `bgjm_start_*()` function.
#' @param n Number of lines to read; defaults to all lines.
#'
#' @return Character vector of log lines.
#' @export
bgjm_read_stdout <- function(job_id, n = -1L) {
  j <- .bgjm_env$jobs[[job_id]]
  if (is.null(j)) {
    stop("Unknown job_id")
  }
  if (file.exists(j$stdout)) {
    readLines(j$stdout, n = n, warn = FALSE)
  } else {
    character()
  }
}

#' Read the captured standard error
#'
#' @param job_id Identifier returned by a `bgjm_start_*()` function.
#' @param n Number of lines to read; defaults to all lines.
#'
#' @return Character vector of log lines.
#' @export
bgjm_read_stderr <- function(job_id, n = -1L) {
  j <- .bgjm_env$jobs[[job_id]]
  if (is.null(j)) {
    stop("Unknown job_id")
  }
  if (file.exists(j$stderr)) {
    readLines(j$stderr, n = n, warn = FALSE)
  } else {
    character()
  }
}
