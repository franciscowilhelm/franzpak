#' Background job registry for franzpak (mirai engine)
#'
#' These helpers run potentially long-running model fits in background R
#' processes using the \pkg{mirai} framework, while adding a thin layer that
#' \pkg{mirai} does not provide on its own:
#'
#' * a **named registry** so jobs can be listed, polled, and collected by a
#'   friendly name rather than by holding `mirai` objects directly, and
#' * **per-job filesystem management** -- each job gets its own directory, its
#'   stdout/stderr are captured to log files, and any files written during the
#'   run (e.g. Mplus `.inp`/`.dat`/`.out`) are relocated into an `artifacts/`
#'   sub-directory.
#'
#' Execution, scheduling, cancellation, and timeouts are delegated to
#' \pkg{mirai}. Jobs run on a dedicated daemon pool (compute profile
#' `"franzpak"`) that is started lazily on first use; see [bgjm_daemons()].
#'
#' @keywords internal
#' @name bgjm
NULL

# Internal registry -------------------------------------------------------------

.bgjm_env <- new.env(parent = emptyenv())
.bgjm_env$jobs <- list()
.bgjm_env$daemons_set <- FALSE
.bgjm_env$n_default <- 2L
.bgjm_compute <- "franzpak"

#' Return the left operand if not `NULL`
#'
#' @param x Left-hand side.
#' @param y Right-hand side.
#' @return `x` when it is not `NULL`, otherwise `y`.
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Generate a timestamped job identifier
#'
#' @param prefix Character prefix prepended to the identifier.
#' @return A length-one character vector containing the identifier.
#' @keywords internal
#' @noRd
.bgjm_now_id <- function(prefix = "job") {
  paste0(
    prefix, "-",
    format(Sys.time(), "%Y%m%d-%H%M%S"), "-",
    substr(as.hexmode(sample.int(.Machine$integer.max, 1)), 1, 6)
  )
}

#' Generate a unique job ID from a user name or an auto-generated one
#'
#' If `job_name` is supplied it is used verbatim when unique, otherwise a
#' `-1`, `-2`, ... suffix is appended. When `NULL`, a timestamped ID is used.
#'
#' @param job_name Optional user-provided job name.
#' @return A unique job identifier string.
#' @keywords internal
#' @noRd
.bgjm_make_unique_id <- function(job_name = NULL) {
  if (is.null(job_name)) {
    return(.bgjm_now_id("job"))
  }
  if (!job_name %in% names(.bgjm_env$jobs)) {
    return(job_name)
  }
  i <- 1
  while (TRUE) {
    candidate <- paste0(job_name, "-", i)
    if (!candidate %in% names(.bgjm_env$jobs)) {
      return(candidate)
    }
    i <- i + 1
  }
}

#' Fetch a registered job or stop
#' @param job_id Identifier.
#' @keywords internal
#' @noRd
.bgjm_get <- function(job_id) {
  j <- .bgjm_env$jobs[[job_id]]
  if (is.null(j)) {
    stop("Unknown job_id: ", job_id)
  }
  j
}

# Daemon pool -------------------------------------------------------------------

#' Configure the franzpak background-job daemon pool
#'
#' Background jobs run on a dedicated \pkg{mirai} daemon pool (compute profile
#' `"franzpak"`), kept separate from any daemons you manage yourself. The pool
#' is started automatically on the first `bgjm_start_*()` call; call this
#' function to choose the number of workers explicitly, or to shut the pool
#' down with `n = 0`.
#'
#' A persistent pool (rather than transient per-call processes) is used so that
#' [bgjm_kill()] and \pkg{mirai} timeouts -- which require a dispatcher -- work,
#' and so that package startup costs are amortised across jobs.
#'
#' @param n Number of background workers. `0` shuts the pool down.
#' @param ... Further arguments passed to [mirai::daemons()] (e.g. `seed`,
#'   `memory`).
#'
#' @return Invisibly, `n`.
#' @seealso [mirai::daemons()]
#' @export
bgjm_daemons <- function(n = 2L, ...) {
  mirai::daemons(n, ..., .compute = .bgjm_compute)
  .bgjm_env$daemons_set <- n > 0
  invisible(n)
}

#' Ensure the daemon pool is running
#' @keywords internal
#' @noRd
.bgjm_ensure_daemons <- function() {
  if (!isTRUE(.bgjm_env$daemons_set)) {
    n <- getOption("franzpak.bgjm_daemons", .bgjm_env$n_default)
    mirai::daemons(n, .compute = .bgjm_compute)
    .bgjm_env$daemons_set <- TRUE
  }
  invisible(TRUE)
}

# Job creation & submission -----------------------------------------------------

#' Create the on-disk skeleton for a new job
#'
#' @param job_name Optional friendly name.
#' @param output_base Directory under which the per-job folder is created.
#' @return A named list of job metadata (id, directories, log paths).
#' @keywords internal
#' @noRd
.bgjm_new_job <- function(job_name = NULL, output_base = tempdir()) {
  job_id <- .bgjm_make_unique_id(job_name)
  job_dir <- file.path(normalizePath(output_base, mustWork = FALSE), job_id)
  art_dir <- file.path(job_dir, "artifacts")
  tmp_dir <- file.path(job_dir, "tmp")
  dir.create(art_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(tmp_dir, showWarnings = FALSE)
  list(
    id = job_id,
    name = job_name %||% job_id,
    dir = job_dir,
    artifacts_dir = art_dir,
    tmp_dir = tmp_dir,
    stdout = file.path(job_dir, "stdout.log"),
    stderr = file.path(job_dir, "stderr.log")
  )
}

#' Body evaluated on the daemon for every job
#'
#' Sets the working directory to the job folder, captures stdout/stderr to log
#' files, runs `target_fun` (wrapped in [try()]), and relocates any files the
#' call wrote into the artifacts directory. Self-contained: references only base
#' R and namespace-qualified calls inside `target_fun`, so it is safe to ship to
#' a clean daemon.
#'
#' @keywords internal
#' @noRd
.bgjm_run_in_daemon <- function(target_fun, args, job_dir, art_dir, tmp_dir,
                                stdout_log, stderr_log, packages, seed, capture) {
  if (length(packages)) {
    for (p in packages) {
      suppressPackageStartupMessages(require(p, character.only = TRUE))
    }
  }

  old_wd <- setwd(job_dir)
  on.exit(setwd(old_wd), add = TRUE)

  old_tmp <- Sys.getenv("TMPDIR", unset = NA)
  Sys.setenv(TMPDIR = tmp_dir)
  on.exit(
    if (is.na(old_tmp)) Sys.unsetenv("TMPDIR") else Sys.setenv(TMPDIR = old_tmp),
    add = TRUE
  )

  if (!is.null(seed)) {
    set.seed(seed)
  }

  before <- character(0)
  if (capture) {
    before <- list.files(
      job_dir, all.files = TRUE, recursive = TRUE, include.dirs = FALSE
    )
  }

  out_con <- file(stdout_log, open = "wt")
  msg_con <- file(stderr_log, open = "wt")
  sink(out_con, type = "output")
  sink(msg_con, type = "message")
  res <- try(do.call(target_fun, args), silent = TRUE)
  sink(type = "message")
  sink(type = "output")
  close(out_con)
  close(msg_con)

  moved <- character(0)
  if (capture) {
    after <- list.files(
      job_dir, all.files = TRUE, recursive = TRUE, include.dirs = FALSE
    )
    new_rel <- setdiff(after, before)
    new_rel <- new_rel[!grepl("^tmp(/|$)|^artifacts(/|$)", new_rel)]
    new_rel <- new_rel[
      !basename(new_rel) %in% c(basename(stdout_log), basename(stderr_log))
    ]
    for (rel in new_rel) {
      src <- file.path(job_dir, rel)
      if (file.exists(src) && !dir.exists(src)) {
        dst <- file.path(art_dir, basename(rel))
        ok <- tryCatch(
          file.rename(src, dst),
          warning = function(w) FALSE, error = function(e) FALSE
        )
        if (!ok) {
          ok2 <- tryCatch(
            file.copy(src, dst, overwrite = TRUE),
            warning = function(w) FALSE, error = function(e) FALSE
          )
          if (isTRUE(ok2)) unlink(src)
        }
        if (file.exists(dst)) moved <- c(moved, dst)
      }
    }
  }

  list(
    ok = !inherits(res, "try-error"),
    result = if (inherits(res, "try-error")) NULL else res,
    error = if (inherits(res, "try-error")) as.character(res) else NULL,
    job_dir = job_dir,
    artifacts_dir = art_dir,
    artifact_files = moved
  )
}

#' Submit a prepared job to the daemon pool and register it
#'
#' @param meta Metadata list from [.bgjm_new_job()].
#' @param target_fun A self-contained function evaluated on the daemon.
#' @param args Named list of arguments for `target_fun`.
#' @param packages Packages to load on the daemon before running.
#' @param seed Optional integer seed.
#' @param capture Whether to relocate written files into `artifacts/`.
#' @return Invisible job id.
#' @keywords internal
#' @noRd
.bgjm_submit <- function(meta, target_fun, args, packages = character(),
                         seed = NULL, capture = TRUE) {
  .bgjm_ensure_daemons()

  # Detach both functions from the package namespace so they serialise to a
  # clean daemon without requiring franzpak to be installed/loaded there. Both
  # use only base R plus namespace-qualified (`pkg::fun`) calls, so baseenv()
  # is sufficient for lookup.
  run_fun <- .bgjm_run_in_daemon
  environment(run_fun) <- baseenv()
  environment(target_fun) <- baseenv()

  m <- mirai::mirai(
    run_fun(
      target_fun = target_fun, args = args, job_dir = job_dir,
      art_dir = art_dir, tmp_dir = tmp_dir, stdout_log = stdout_log,
      stderr_log = stderr_log, packages = packages, seed = seed,
      capture = capture
    ),
    .args = list(
      run_fun = run_fun,
      target_fun = target_fun, args = args, job_dir = meta$dir,
      art_dir = meta$artifacts_dir, tmp_dir = meta$tmp_dir,
      stdout_log = meta$stdout, stderr_log = meta$stderr,
      packages = packages, seed = seed, capture = capture
    ),
    .compute = .bgjm_compute
  )

  .bgjm_env$jobs[[meta$id]] <- list(
    id = meta$id,
    name = meta$name,
    dir = meta$dir,
    artifacts_dir = meta$artifacts_dir,
    stdout = meta$stdout,
    stderr = meta$stderr,
    mirai = m,
    started = Sys.time()
  )

  invisible(meta$id)
}

# Launchers ---------------------------------------------------------------------

#' Start a lavaan background job
#'
#' Runs one of `lavaan::sem()`, `lavaan::cfa()`, or `lavaan::lavaan()` with the
#' provided syntax and data on a background daemon.
#'
#' @param model_syntax Character vector containing lavaan model syntax.
#' @param data Data frame passed to the lavaan function.
#' @param lavaan_fun One of `"sem"`, `"cfa"`, or `"lavaan"`.
#' @param ... Additional arguments forwarded to the selected lavaan function
#'   (e.g. `se = "bootstrap"`, `bootstrap = 1000`).
#' @param job_name Optional friendly job name.
#' @param output_base Directory where the job folder is created.
#' @param seed Optional random seed.
#'
#' @return Invisible job identifier string.
#' @seealso [bgjm_collect()], [bgjm_status()]
#' @export
#' @examplesIf interactive() && requireNamespace("lavaan", quietly = TRUE)
#' \dontrun{
#' job <- bgjm_start_lavaan(
#'   model_syntax = "f1 =~ x1 + x2 + x3",
#'   data = lavaan::HolzingerSwineford1939,
#'   lavaan_fun = "cfa"
#' )
#' fit <- bgjm_collect(job)$result
#' }
bgjm_start_lavaan <- function(model_syntax, data,
                              lavaan_fun = c("sem", "cfa", "lavaan"), ...,
                              job_name = NULL, output_base = tempdir(),
                              seed = NULL) {
  lavaan_fun <- match.arg(lavaan_fun)

  target_fun <- function(.syntax, .data, .fn, ...) {
    f <- get(.fn, asNamespace("lavaan"))
    f(.syntax, data = .data, ...)
  }

  meta <- .bgjm_new_job(
    job_name %||% sprintf("lavaan-%s", lavaan_fun), output_base
  )
  .bgjm_submit(
    meta, target_fun,
    list(.syntax = model_syntax, .data = data, .fn = lavaan_fun, ...),
    packages = "lavaan", seed = seed
  )
}

#' Start a tidyLPA background job
#'
#' Runs [tidyLPA::estimate_profiles()] with the supplied data and configuration
#' on a background daemon. When `package = "Mplus"`, any files written by the
#' run are captured into the job's `artifacts/` directory.
#'
#' @param data A data frame passed to `estimate_profiles()`.
#' @param n_profiles Number of profiles to estimate.
#' @param ... Additional arguments forwarded to `estimate_profiles()` (e.g.
#'   `models`, `variances`, `covariances`).
#' @param package `"mclust"` or `"Mplus"` backend (tidyLPA >= 1.0 naming).
#' @param job_name Optional friendly job name.
#' @param output_base Directory where the job folder is created.
#' @param seed Optional random seed.
#'
#' @return Invisible job identifier string.
#' @seealso [bgjm_collect()], [bgjm_start_mplus()]
#' @export
#' @examplesIf interactive() && requireNamespace("tidyLPA", quietly = TRUE)
#' \dontrun{
#' job <- bgjm_start_tidylpa(
#'   data = iris[, 1:4], n_profiles = 3, models = 1, package = "mclust"
#' )
#' res <- bgjm_collect(job)$result
#' }
bgjm_start_tidylpa <- function(data, n_profiles, ...,
                               package = c("mclust", "Mplus"),
                               job_name = NULL, output_base = tempdir(),
                               seed = NULL) {
  package <- match.arg(package)

  target_fun <- function(.data, .n, .package, ...) {
    tidyLPA::estimate_profiles(.data, n_profiles = .n, package = .package, ...)
  }

  meta <- .bgjm_new_job(
    job_name %||% sprintf("tidylpa-n%d-%s", n_profiles, package), output_base
  )
  .bgjm_submit(
    meta, target_fun,
    list(.data = data, .n = n_profiles, .package = package, ...),
    packages = unique(c("tidyLPA", if (package == "Mplus") "MplusAutomation")),
    seed = seed
  )
}

#' Start an Mplus (`MplusAutomation::runModels`) background job
#'
#' Runs an existing Mplus input on a background daemon and captures its output.
#' The input file(s) are copied into the job's working directory, Mplus is run
#' there via [MplusAutomation::runModels()], and the resulting `.out` (plus the
#' Mplus run log `mplus_run.log`) are relocated into the job's `artifacts/`
#' directory. The job result is the parsed model object from
#' [MplusAutomation::readModels()].
#'
#' Mplus's own console output is streamed via `showOutput = TRUE`,
#' `quiet = FALSE` and -- on Unix/macOS, where `runModels()` routes it through
#' R -- captured to the job's stdout log (readable with [bgjm_read_stdout()]).
#' The authoritative record remains the `.out` artifact.
#'
#' @param target Path to an Mplus `.inp` file, or a directory containing `.inp`
#'   (and accompanying `.dat`) files. Staged into the job directory before
#'   running, so the original files are left untouched.
#' @param ... Further arguments forwarded to [MplusAutomation::runModels()]
#'   (e.g. `recursive`, `replaceOutfile`, `Mplus_command`). Do not pass
#'   `target`, `logFile`, `showOutput`, or `quiet`; these are set internally.
#' @param job_name Optional friendly job name.
#' @param output_base Directory where the job folder is created.
#' @param seed Optional random seed.
#'
#' @return Invisible job identifier string.
#' @seealso [bgjm_collect()], [MplusAutomation::runModels()]
#' @export
#' @examplesIf interactive() && requireNamespace("MplusAutomation", quietly = TRUE)
#' \dontrun{
#' # Given a directory holding model.inp and its data file:
#' job <- bgjm_start_mplus(target = "path/to/model_dir")
#' out <- bgjm_collect(job)
#' list.files(out$artifacts_dir)        # model.out, mplus_run.log
#' out$result                           # parsed via MplusAutomation::readModels()
#' }
bgjm_start_mplus <- function(target, ..., job_name = NULL,
                             output_base = tempdir(), seed = NULL) {
  if (!file.exists(target)) {
    stop("`target` does not exist: ", target)
  }

  meta <- .bgjm_new_job(job_name %||% "mplus", output_base)

  if (dir.exists(target)) {
    files <- list.files(
      target, pattern = "\\.(inp|dat|csv|txt)$",
      full.names = TRUE, ignore.case = TRUE
    )
  } else {
    d <- dirname(target)
    files <- c(
      target,
      list.files(d, pattern = "\\.dat$", full.names = TRUE, ignore.case = TRUE)
    )
  }
  if (!length(files)) {
    stop("No Mplus input files (.inp/.dat) found at: ", target)
  }
  file.copy(files, meta$dir, overwrite = TRUE)

  target_fun <- function(.run_dir, ...) {
    MplusAutomation::runModels(
      target = .run_dir,
      logFile = file.path(.run_dir, "mplus_run.log"),
      showOutput = TRUE,
      quiet = FALSE,
      ...
    )
    MplusAutomation::readModels(.run_dir)
  }

  .bgjm_submit(
    meta, target_fun, list(.run_dir = meta$dir, ...),
    packages = "MplusAutomation", seed = seed
  )
}

# Registry & control ------------------------------------------------------------

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
    data.frame(
      id = j$id,
      name = j$name,
      status = bgjm_status(j$id),
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
  j <- .bgjm_get(job_id)
  m <- j$mirai
  if (mirai::unresolved(m)) {
    return("running")
  }
  d <- m$data
  # is_error_value() also covers interrupts (killed jobs) and timeouts, which
  # is_mirai_error() does not.
  if (mirai::is_error_value(d) || (is.list(d) && isFALSE(d$ok))) {
    "error"
  } else {
    "finished"
  }
}

#' Collect the result of a background job
#'
#' Blocks until the job has resolved, then returns its result and metadata.
#'
#' @param job_id Identifier returned by a `bgjm_start_*()` function.
#' @param auto_remove If `TRUE`, remove the job (and its directory) from the
#'   registry after collection.
#'
#' @return A list with the job `result` plus metadata (`artifacts_dir`,
#'   `artifact_files`, `job_dir`, `stdout`, `stderr`).
#' @export
bgjm_collect <- function(job_id, auto_remove = FALSE) {
  j <- .bgjm_get(job_id)
  d <- mirai::collect_mirai(j$mirai)

  if (mirai::is_error_value(d)) {
    reason <- if (mirai::is_mirai_error(d)) {
      conditionMessage(d)
    } else if (mirai::is_mirai_interrupt(d)) {
      "job was interrupted (killed)"
    } else {
      "job failed (timeout or daemon communication error)"
    }
    stop(sprintf(
      "Background job '%s' failed: %s\n  stdout: %s\n  stderr: %s",
      job_id, reason, j$stdout, j$stderr
    ))
  }
  if (is.list(d) && isFALSE(d$ok)) {
    stop(sprintf(
      "Background job '%s' returned an error: %s\n  stderr: %s",
      job_id, d$error %||% "see logs", j$stderr
    ))
  }

  out <- list(
    id = job_id,
    name = j$name,
    result = d$result,
    artifacts_dir = d$artifacts_dir,
    artifact_files = d$artifact_files,
    job_dir = d$job_dir,
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
#' Requests cooperative cancellation via [mirai::stop_mirai()]. Requires the
#' daemon pool (dispatcher), which is the default.
#'
#' @param job_id Identifier returned by a `bgjm_start_*()` function.
#'
#' @return Invisible logical from [mirai::stop_mirai()].
#' @export
bgjm_kill <- function(job_id) {
  j <- .bgjm_get(job_id)
  invisible(mirai::stop_mirai(j$mirai))
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
  if (mirai::unresolved(j$mirai)) {
    stop("Cannot remove a running job; kill or collect it first.")
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
  j <- .bgjm_get(job_id)
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
  j <- .bgjm_get(job_id)
  if (file.exists(j$stderr)) {
    readLines(j$stderr, n = n, warn = FALSE)
  } else {
    character()
  }
}
