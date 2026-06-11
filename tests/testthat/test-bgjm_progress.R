# NOTE: bgjm_progress() and its parsers are internal groundwork and not yet
# functional end-to-end -- Mplus's live console stream is not currently captured
# into the job's stdout log (see R/bgjm_progress.R header). These tests exercise
# the parser logic against captured fixtures, which remains valid for when the
# stdout capture is wired up.

fx <- function(...) test_path("fixtures", "mplus_progress", ...)

# .inp settings parser ---------------------------------------------------------

test_that(".bgjm_read_inp reads Bayes regimes and the BITERATIONS/FBITERATIONS distinction", {
  default <- .bgjm_read_inp(fx("bayes_default.inp"))
  expect_true(default$is_bayes)
  expect_false(default$is_mixture)
  expect_true(is.na(default$fbiterations))
  expect_true(is.na(default$biterations_max))

  fixed <- .bgjm_read_inp(fx("bayes_fbiter.inp"))
  expect_equal(fixed$fbiterations, 10000)
  # the \\b guard must not let FBITERATIONS leak into BITERATIONS
  expect_true(is.na(fixed$biterations_max))
  expect_match(fixed$fbiterations_raw, "FBITERATIONS = 10000")

  bounded <- .bgjm_read_inp(fx("bayes_biter.inp"))
  expect_true(is.na(bounded$fbiterations))
  expect_equal(bounded$biterations_max, 30000)
  expect_equal(bounded$biterations_min, 2000)
  expect_equal(bounded$bconvergence, 0.01)
})

test_that(".bgjm_read_inp reads mixture STARTS, CLASSES, and TECH14", {
  s <- .bgjm_read_inp(fx("mixture_starts.inp"))
  expect_true(s$is_mixture)
  expect_false(s$is_bayes)
  expect_equal(s$starts_init, 500)
  expect_equal(s$starts_final, 50)
  expect_equal(s$classes, 3)
  expect_true(s$tech14)
  expect_true(is.na(s$lrtbootstrap))
})

# Bayes stream parser ----------------------------------------------------------

test_that("Bayes parser reports iteration + PSR and only shows a % under FBITERATIONS", {
  lines <- readLines(fx("bayes_stdout.txt"), warn = FALSE)

  default <- .bgjm_progress_bayes(lines, .bgjm_read_inp(fx("bayes_default.inp")))
  expect_equal(default$kind, "bayes")
  expect_equal(default$current, 1000)
  expect_equal(default$psr, 1.133)
  expect_true(is.na(default$total))
  expect_match(default$message, "Neither FBITERATIONS nor BITERATIONS")
  expect_no_match(default$message, "%")

  fixed <- .bgjm_progress_bayes(lines, .bgjm_read_inp(fx("bayes_fbiter.inp")))
  expect_equal(fixed$total, 10000)
  expect_match(fixed$message, "10%")
  expect_match(fixed$message, "FBITERATIONS = 10000")

  bounded <- .bgjm_progress_bayes(lines, .bgjm_read_inp(fx("bayes_biter.inp")))
  expect_true(is.na(bounded$total))
  expect_match(bounded$message, "minimum of 2,000 and a maximum of 30,000")
  expect_no_match(bounded$message, "%")
})

# Mixture stream parser --------------------------------------------------------

test_that("mixture parser tracks random starts against STARTS", {
  lines <- readLines(fx("mixture_starts_stdout.txt"), warn = FALSE)
  p <- .bgjm_progress_mixture(lines, .bgjm_read_inp(fx("mixture_starts.inp")))
  expect_equal(p$phase, "random starts")
  expect_equal(p$current, 6)
  expect_equal(p$total, 500)
  expect_equal(p$short, "LPA: start 6/500")
})

test_that("mixture parser recognises the (k-1)-class search", {
  lines <- readLines(fx("mixture_h0_stdout.txt"), warn = FALSE)
  p <- .bgjm_progress_mixture(lines, .bgjm_read_inp(fx("mixture_starts.inp")))
  expect_equal(p$phase, "k-1 class model")
  expect_equal(p$current, 7)
  expect_match(p$message, "one fewer class")
})

test_that("mixture parser tracks TECH14 bootstrap draws with an adaptive total", {
  lines <- readLines(fx("mixture_bootstrap_stdout.txt"), warn = FALSE)
  p <- .bgjm_progress_mixture(lines, .bgjm_read_inp(fx("mixture_starts.inp")))
  expect_equal(p$phase, "bootstrap")
  expect_equal(p$current, 5)
  expect_true(is.na(p$total))
  expect_match(p$message, "adaptively")
  expect_match(p$message, "3-class")
})

# End-to-end via a fake registry entry (no mirai needed) -----------------------

# Build a job dir holding `inp` + a stdout log, register a fake job, and return
# its id. The registry entry is removed on exit of the calling test.
register_fake_job <- function(env, inp, stdout_fixture, id = "fake") {
  dir <- withr::local_tempdir(.local_envir = env)
  file.copy(inp, file.path(dir, basename(inp)))
  log <- file.path(dir, "stdout.log")
  file.copy(stdout_fixture, log)
  .bgjm_env$jobs[[id]] <- list(
    id = id, name = id, dir = dir,
    artifacts_dir = file.path(dir, "artifacts"),
    stdout = log, stderr = file.path(dir, "stderr.log")
  )
  withr::defer(.bgjm_env$jobs[[id]] <- NULL, envir = env)
  id
}

test_that("bgjm_progress() dispatches end-to-end and finds the matching .inp", {
  id <- register_fake_job(
    environment(),
    fx("mixture_starts.inp"), fx("mixture_starts_stdout.txt"),
    id = "fake-mix"
  )
  p <- bgjm_progress(id)
  expect_equal(p$kind, "mixture")
  expect_equal(p$current, 6)
  expect_equal(p$job_id, "fake-mix")

  id2 <- register_fake_job(
    environment(),
    fx("bayes_fbiter.inp"), fx("bayes_stdout.txt"),
    id = "fake-bayes"
  )
  p2 <- bgjm_progress(id2)
  expect_equal(p2$kind, "bayes")
  expect_match(p2$message, "10%")
})

test_that("bgjm_progress() degrades gracefully for non-Mplus / empty logs", {
  dir <- withr::local_tempdir()
  log <- file.path(dir, "stdout.log")
  writeLines(c("lavaan happily converged", "no mplus markers here"), log)
  .bgjm_env$jobs[["fake-lav"]] <- list(
    id = "fake-lav", name = "fake-lav", dir = dir,
    artifacts_dir = file.path(dir, "artifacts"),
    stdout = log, stderr = file.path(dir, "stderr.log")
  )
  withr::defer(.bgjm_env$jobs[["fake-lav"]] <- NULL)

  p <- bgjm_progress("fake-lav")
  expect_equal(p$kind, "unknown")
  expect_match(p$message, "Mplus Bayes and mixture")
})
