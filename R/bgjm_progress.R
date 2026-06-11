# Mplus progress tracking for background jobs ----------------------------------
#
# STATUS: groundwork only -- NOT functional, and deliberately unexported.
# The parsers below are correct (covered by tests against captured fixtures),
# but they have nothing to read at runtime: Mplus's live TECH8 console stream is
# written by the Mplus subprocess to the daemon's OS-level stdout and is NOT
# captured into the job's stdout log (our runner's `sink()` redirects only R's
# own output, not a child process's fd 1 -- confirmed by a live probe). Wiring
# this up requires an OS-level stdout redirect in the Mplus runner; see the
# future-work NOTE in R/bgjm.R (bgjm_start_mplus). Until then bgjm_progress()
# would report "unknown" for a whole run, so it stays internal.
#
# Mplus streams human-readable progress to *stdout* during a run (TECH8 output).
# This file parses the tail of that stream (once it is actually captured) into
# an informative, non-blocking status.
#
# Two estimators are recognised so far:
#
# * BAYES -- one row per 100 iterations: `<iteration> <PSR> <param#> <time>
#   <total>`. The Potential Scale Reduction (PSR) trending toward 1 is the real
#   progress signal. A meaningful *percentage* exists only when the input fixes
#   the run length with FBITERATIONS; with BITERATIONS the run is bounded but
#   convergence-driven, and with neither it is governed purely by the BCONVERGENCE
#   criterion (no fixed total).
# * MIXTURE (LPA/LCA) -- random-starts search prints `STARTING VALUE SET k`
#   (target = STARTS); an optional TECH14 bootstrap LRT then prints
#   `(ESTIMATION WITH) BOOTSTRAP DRAW (NUMBER) k`, whose total Mplus usually
#   decides adaptively ("Varies") unless LRTBOOTSTRAP fixes it.
#
# Run-length *targets* come from the staged `.inp` (parsed here), never the
# `.out`: see the NOTE in bgjm_start_mplus() for why the `.out` is useless for
# progress. The design is deliberately additive -- new estimators slot into the
# dispatch in bgjm_progress() -- and every parser is guarded so a malformed,
# empty, or truncated log degrades to "no progress information yet" rather than
# erroring.

# .inp settings reader ---------------------------------------------------------

#' Default (empty) Mplus settings list
#' @keywords internal
#' @noRd
.bgjm_default_settings <- function() {
  list(
    is_bayes = FALSE, is_mixture = FALSE, tech14 = FALSE,
    fbiterations = NA_real_, fbiterations_raw = NA_character_,
    biterations_max = NA_real_, biterations_min = NA_real_,
    biterations_raw = NA_character_,
    bconvergence = NA_real_,
    starts_init = NA_real_, starts_final = NA_real_,
    lrtbootstrap = NA_real_, classes = NA_real_
  )
}

#' Capture the first regex group, or `NA`
#' @keywords internal
#' @noRd
.bgjm_cap <- function(pattern, x, group = 1L) {
  g <- regmatches(x, regexec(pattern, x, perl = TRUE))[[1]]
  if (length(g) < group + 1L) {
    return(NA_character_)
  }
  v <- g[group + 1L]
  if (is.na(v) || !nzchar(v)) NA_character_ else v
}

#' Capture the whole regex match, or `NA`
#' @keywords internal
#' @noRd
.bgjm_cap0 <- function(pattern, x) {
  m <- regmatches(x, regexpr(pattern, x, perl = TRUE))
  if (length(m) && nzchar(m)) m else NA_character_
}

#' Parse the run-length-relevant settings from an Mplus `.inp`
#'
#' Strips `!` comments, upper-cases, and collapses whitespace so multi-line
#' commands match. Note the `\\b` before BITERATIONS: it prevents a spurious
#' match inside FBITERATIONS (there is no word boundary between `F` and `B`).
#'
#' @keywords internal
#' @noRd
.bgjm_read_inp <- function(inp_path) {
  txt <- tryCatch(readLines(inp_path, warn = FALSE), error = function(e) character())
  if (!length(txt)) {
    return(NULL)
  }
  one <- toupper(paste(sub("!.*$", "", txt), collapse = " "))
  one <- gsub("[[:space:]]+", " ", one)

  numify <- function(x) if (is.na(x)) NA_real_ else as.numeric(x)

  s <- .bgjm_default_settings()
  s$is_bayes <- grepl("ESTIMATOR\\s*=\\s*BAYES", one)
  s$is_mixture <- grepl("TYPE\\s*=\\s*MIXTURE", one)
  s$tech14 <- grepl("\\bTECH14\\b", one)

  s$fbiterations <- numify(.bgjm_cap("\\bFBITERATIONS\\s*=\\s*([0-9]+)", one))
  s$fbiterations_raw <- .bgjm_cap0("\\bFBITERATIONS\\s*=\\s*[0-9]+", one)

  s$biterations_max <- numify(.bgjm_cap("\\bBITERATIONS\\s*=\\s*([0-9]+)", one))
  s$biterations_min <- numify(
    .bgjm_cap("\\bBITERATIONS\\s*=\\s*[0-9]+\\s*\\(\\s*([0-9]+)\\s*\\)", one)
  )
  s$biterations_raw <- .bgjm_cap0(
    "\\bBITERATIONS\\s*=\\s*[0-9]+\\s*(?:\\(\\s*[0-9]+\\s*\\))?", one
  )

  s$bconvergence <- numify(.bgjm_cap("\\bBCONVERGENCE\\s*=\\s*([0-9.]+)", one))
  s$starts_init <- numify(.bgjm_cap("\\bSTARTS\\s*=\\s*([0-9]+)", one))
  s$starts_final <- numify(.bgjm_cap("\\bSTARTS\\s*=\\s*[0-9]+\\s+([0-9]+)", one))
  s$lrtbootstrap <- numify(.bgjm_cap("\\bLRTBOOTSTRAP\\s*=\\s*([0-9]+)", one))
  s$classes <- numify(.bgjm_cap("\\bCLASSES\\s*=\\s*\\w+\\s*\\(\\s*([0-9]+)\\s*\\)", one))
  s
}

#' Locate and parse the `.inp` matching a job's running estimator
#'
#' Searches the job's working and artifacts directories, parses every `.inp`,
#' and returns the settings for the one matching `kind` (the most recently
#' modified, if several). Falls back to empty settings when none is found.
#'
#' @keywords internal
#' @noRd
.bgjm_find_inp_settings <- function(job, kind) {
  dirs <- unique(c(job$dir, job$artifacts_dir))
  inps <- unlist(lapply(dirs, function(d) {
    if (!is.null(d) && dir.exists(d)) {
      list.files(d, pattern = "\\.inp$", full.names = TRUE, ignore.case = TRUE)
    } else {
      character()
    }
  }))
  if (!length(inps)) {
    return(.bgjm_default_settings())
  }
  inps <- inps[order(file.info(inps)$mtime)]   # oldest -> newest
  parsed <- Filter(Negate(is.null), lapply(inps, .bgjm_read_inp))
  if (!length(parsed)) {
    return(.bgjm_default_settings())
  }
  matches <- Filter(function(s) {
    switch(kind,
      bayes = isTRUE(s$is_bayes),
      mixture = isTRUE(s$is_mixture),
      TRUE
    )
  }, parsed)
  if (length(matches)) matches[[length(matches)]] else parsed[[length(parsed)]]
}

# Stream parsers ---------------------------------------------------------------

#' Last integer captured by `pattern` across `lines`, or `NA`
#' @keywords internal
#' @noRd
.bgjm_last_int <- function(pattern, lines) {
  hits <- regmatches(lines, regexec(pattern, lines, perl = TRUE))
  vals <- vapply(hits, function(g) {
    if (length(g) >= 2L && nzchar(g[2])) as.numeric(g[2]) else NA_real_
  }, numeric(1))
  vals <- vals[!is.na(vals)]
  if (!length(vals)) NA_real_ else vals[length(vals)]
}

#' Format an integer with thousands separators
#' @keywords internal
#' @noRd
.bgjm_n <- function(x) format(x, big.mark = ",", trim = TRUE, scientific = FALSE)

#' Parse Bayes (TECH8) progress from captured stdout
#' @keywords internal
#' @noRd
.bgjm_progress_bayes <- function(lines, s) {
  # Bayes TECH8 rows: "<iter> <PSR(decimal)> <param#> <time> <total>". The
  # decimal PSR in the 2nd column distinguishes these from mixture EM rows,
  # whose 2nd column is a signed D-exponent loglikelihood (e.g. -0.74D+03).
  rows <- grep("^\\s*[0-9]+\\s+[0-9]+\\.[0-9]+\\s+[0-9]+\\s", lines, value = TRUE)
  if (!length(rows)) {
    return(NULL)
  }
  nums <- as.numeric(strsplit(trimws(rows[length(rows)]), "\\s+")[[1]])
  iter <- nums[1]
  psr <- nums[2]

  short <- sprintf("Bayes: iter %d, PSR %.3f", as.integer(iter), psr)

  if (!is.na(s$fbiterations)) {
    pct <- round(100 * iter / s$fbiterations)
    message <- sprintf(
      paste0("Bayesian estimation, iteration %s. Your input fixes the run ",
             "length (%s), so this is %d%% of the way through. PSR ",
             "convergence diagnostic is now %.3f (values near 1 mean the ",
             "chains agree)."),
      .bgjm_n(iter), s$fbiterations_raw, pct, psr
    )
    total <- s$fbiterations
  } else if (!is.na(s$biterations_max)) {
    rng <- if (!is.na(s$biterations_min)) {
      sprintf("a minimum of %s and a maximum of %s iterations",
              .bgjm_n(s$biterations_min), .bgjm_n(s$biterations_max))
    } else {
      sprintf("a maximum of %s iterations", .bgjm_n(s$biterations_max))
    }
    message <- sprintf(
      paste0("Bayesian estimation, iteration %s (PSR %.3f). Your input sets ",
             "%s, which gives %s; within that range Mplus stops as soon as the ",
             "chains converge, so there is no fixed endpoint."),
      .bgjm_n(iter), psr, s$biterations_raw, rng
    )
    total <- NA_real_
  } else {
    crit <- if (!is.na(s$bconvergence)) {
      sprintf("BCONVERGENCE = %s", format(s$bconvergence))
    } else {
      "the default BCONVERGENCE of 0.05"
    }
    message <- sprintf(
      paste0("Bayesian estimation, iteration %s (PSR %.3f). Neither ",
             "FBITERATIONS nor BITERATIONS is set, so the run length is ",
             "governed by the convergence criterion (%s), not a fixed number ",
             "of iterations: Mplus samples until the PSR is small enough, up to ",
             "a default maximum of 50,000."),
      .bgjm_n(iter), psr, crit
    )
    total <- NA_real_
  }

  list(
    kind = "bayes", phase = "sampling",
    current = iter, total = total, psr = psr,
    short = short, message = message
  )
}

#' Parse mixture (random starts / TECH14 bootstrap) progress from stdout
#' @keywords internal
#' @noRd
.bgjm_progress_mixture <- function(lines, s) {
  # Phase precedence (not line position): the bootstrap stage prints its own
  # internal STARTING VALUE SET lines, so a draw marker anywhere means we are in
  # the bootstrap; an H0 header means the (k-1)-class search; otherwise the main
  # random-starts search.
  has_draw <- any(grepl("BOOTSTRAP DRAW", lines))
  has_h0 <- any(grepl("H0 MODEL WITH ONE LESS CLASS", lines))
  set <- .bgjm_last_int("STARTING VALUE SET\\s+([0-9]+)", lines)

  tgt <- function(n) if (is.na(n)) "" else sprintf(" of %s", .bgjm_n(n))

  if (has_draw) {
    draw <- .bgjm_last_int("BOOTSTRAP DRAW(?:\\s+NUMBER)?\\s+([0-9]+)", lines)
    if (is.na(draw)) {
      return(NULL)
    }
    total_txt <- if (!is.na(s$lrtbootstrap)) {
      sprintf("of %s requested", .bgjm_n(s$lrtbootstrap))
    } else {
      "(Mplus decides the number of draws adaptively, so there is no fixed total)"
    }
    vs <- if (!is.na(s$classes)) {
      sprintf(" comparing the %d-class model against one with fewer classes", s$classes)
    } else {
      ""
    }
    return(list(
      kind = "mixture", phase = "bootstrap",
      current = draw, total = if (is.na(s$lrtbootstrap)) NA_real_ else s$lrtbootstrap,
      psr = NA_real_,
      short = sprintf("LPA: bootstrap draw %d", as.integer(draw)),
      message = sprintf(
        paste0("TECH14 bootstrapped likelihood-ratio test: bootstrap draw %d ",
               "%s. This final stage%s."),
        as.integer(draw), total_txt, vs
      )
    ))
  }

  if (is.na(set)) {
    return(NULL)
  }

  if (has_h0) {
    return(list(
      kind = "mixture", phase = "k-1 class model",
      current = set, total = s$starts_init, psr = NA_real_,
      short = sprintf("LPA: (k-1) start %d", as.integer(set)),
      message = sprintf(
        paste0("Fitting the comparison model with one fewer class (needed for ",
               "the TECH14 test): random-starts set %d%s."),
        as.integer(set), tgt(s$starts_init)
      )
    ))
  }

  list(
    kind = "mixture", phase = "random starts",
    current = set, total = s$starts_init, psr = NA_real_,
    short = sprintf("LPA: start %d%s", as.integer(set),
                    if (is.na(s$starts_init)) "" else sprintf("/%s", .bgjm_n(s$starts_init))),
    message = sprintf(
      paste0("Random-starts search: starting-value set %d%s. Mixture models try ",
             "many random starts to avoid local maxima before settling on the ",
             "best solution."),
      as.integer(set), tgt(s$starts_init)
    )
  )
}

# Public entry point -----------------------------------------------------------

#' Report informative progress for a running Mplus background job (INTERNAL,
#' not yet functional)
#'
#' @description
#' **Not wired up -- intentionally unexported.** The stream parsers here are
#' correct and tested against fixtures, but they have no live data to read: an
#' empirical probe showed that Mplus's TECH8 console stream is written by the
#' Mplus subprocess to the daemon's OS-level stdout and is **not** captured into
#' the job's stdout log (our runner's `sink()` only redirects R's own output,
#' not a child process's file descriptor). So in normal use this would report
#' `"unknown"` for an entire run. It is kept, de-exported, as the foundation for
#' a future feature.
#'
#' To make it work, the Mplus runner in [bgjm_start_mplus()] must redirect the
#' subprocess stdout to the log at the OS level (e.g. `system2(..., stdout =
#' stdout_log)`) instead of relying on `runModels(showOutput = TRUE)` + `sink`.
#' See the future-work NOTE in `R/bgjm.R` (Mplus runner) for the full rationale.
#'
#' Once that capture is in place, this reads the tail of the job's stdout log
#' and turns Mplus's progress output into a plain summary: a Bayesian run's MCMC
#' iteration and PSR convergence diagnostic, or which random start / bootstrap
#' draw a mixture (LPA/LCA) model is on. Run-length *targets* come from the
#' staged `.inp` (a true Bayes percentage only under `FBITERATIONS`; mixture
#' starts against `STARTS`; `TECH14` draws usually adaptive).
#'
#' @param job_id Identifier returned by a `bgjm_start_*()` function.
#' @param n_tail Number of trailing stdout lines to scan (default 1000).
#'
#' @return A list with `kind`, `phase`, `current`, `total`, `psr`, `short`, and
#'   `message`.
#' @seealso [bgjm_status()], [bgjm_read_stdout()], [bgjm_collect()]
#' @keywords internal
#' @noRd
bgjm_progress <- function(job_id, n_tail = 1000L) {
  j <- .bgjm_get(job_id)

  unknown <- function(msg) {
    list(
      kind = "unknown", phase = NA_character_,
      current = NA_real_, total = NA_real_, psr = NA_real_,
      short = "no progress info", message = msg
    )
  }

  lines <- bgjm_read_stdout(job_id)
  if (length(lines) > n_tail) {
    lines <- utils::tail(lines, n_tail)
  }
  if (!length(lines)) {
    return(unknown(
      "No output captured yet. The job may still be starting, or stdout is not routed through R on this platform (Windows)."
    ))
  }

  kind <- if (any(grepl("BAYES ESTIMATION", lines))) {
    "bayes"
  } else if (any(grepl("STARTING VALUE SET|BOOTSTRAP DRAW", lines))) {
    "mixture"
  } else {
    "unknown"
  }

  if (kind == "unknown") {
    return(unknown(
      "No Mplus progress markers found yet. Progress tracking currently covers Mplus Bayes and mixture (LPA/LCA) models; use bgjm_status() or bgjm_read_stdout() otherwise."
    ))
  }

  settings <- .bgjm_find_inp_settings(j, kind)
  parser <- switch(kind,
    bayes = .bgjm_progress_bayes,
    mixture = .bgjm_progress_mixture
  )
  prog <- tryCatch(parser(lines, settings), error = function(e) NULL)
  if (is.null(prog)) {
    return(unknown(
      "Mplus is running but has not yet printed a parseable progress marker."
    ))
  }
  prog$job_id <- job_id
  prog
}
