# Background-job tests (mirai engine).
#
# All tests need mirai. Model-package tests skip when the package (or the Mplus
# executable) is unavailable. bgjm_collect() blocks until resolution, so no
# manual polling/waiting is required.

skip_if_no_mirai <- function() {
  skip_if_not_installed("mirai")
}

mplus_available <- function() {
  if (!requireNamespace("MplusAutomation", quietly = TRUE)) {
    return(FALSE)
  }
  tryCatch({
    MplusAutomation::detectMplus()
    TRUE
  }, error = function(e) FALSE)
}

# Build a minimal Mplus .inp + .dat fixture *without* running Mplus
# (mplusModeler(run = 0)). Returns the directory holding the staged input.
make_mplus_fixture <- function(dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  set.seed(1)
  n <- 60
  df <- data.frame(
    y = rnorm(n),
    x1 = rnorm(n),
    x2 = rnorm(n)
  )
  obj <- MplusAutomation::mplusObject(
    TITLE = "bgjm fixture;",
    MODEL = "y ON x1 x2;",
    usevariables = c("y", "x1", "x2"),
    rdata = df
  )
  suppressMessages(MplusAutomation::mplusModeler(
    obj,
    dataout = file.path(dir, "fixture.dat"),
    modelout = file.path(dir, "fixture.inp"),
    run = 0L,
    writeData = "always",
    hashfilename = FALSE
  ))
  dir
}

test_that("generic job captures artifacts and returns its result", {
  skip_if_no_mirai()
  output_base <- file.path(tempdir(), sprintf("bgjm-generic-%d", Sys.getpid()))

  meta <- franzpak:::.bgjm_new_job("generic-test", output_base)
  target_fun <- function(.n) {
    writeLines("background side effect", paste0("artifact-", .n, ".txt"))
    sum(seq_len(.n))
  }
  job_id <- franzpak:::.bgjm_submit(meta, target_fun, list(.n = 5))

  on.exit(try(bgjm_remove(job_id, delete_dir = TRUE), silent = TRUE), add = TRUE)

  collected <- bgjm_collect(job_id)
  expect_equal(collected$result, sum(seq_len(5)))
  expect_length(collected$artifact_files, 1)
  expect_true(all(file.exists(collected$artifact_files)))
  expect_match(collected$artifact_files, "artifact-5\\.txt$")

  listed <- bgjm_list()
  expect_true(job_id %in% listed$id)
  expect_equal(listed$status[listed$id == job_id], "finished")
})

test_that("stdout and stderr are captured to per-job logs", {
  skip_if_no_mirai()
  output_base <- file.path(tempdir(), sprintf("bgjm-logs-%d", Sys.getpid()))

  meta <- franzpak:::.bgjm_new_job("logs-test", output_base)
  target_fun <- function() {
    cat("this is stdout output\n")
    message("this is a message")
    warning("this is a warning")
    123
  }
  job_id <- franzpak:::.bgjm_submit(meta, target_fun, list())
  on.exit(try(bgjm_remove(job_id, delete_dir = TRUE), silent = TRUE), add = TRUE)

  collected <- bgjm_collect(job_id)
  expect_equal(collected$result, 123)

  stdout_lines <- bgjm_read_stdout(job_id)
  stderr_lines <- bgjm_read_stderr(job_id)
  expect_true(any(grepl("stdout output", stdout_lines)))
  expect_true(any(grepl("message|warning", stderr_lines, ignore.case = TRUE)))
})

test_that("a failing job reports error status and errors on collect", {
  skip_if_no_mirai()
  output_base <- file.path(tempdir(), sprintf("bgjm-fail-%d", Sys.getpid()))

  meta <- franzpak:::.bgjm_new_job("fail-test", output_base)
  job_id <- franzpak:::.bgjm_submit(
    meta, function() stop("boom"), list()
  )
  on.exit(try(bgjm_remove(job_id, delete_dir = TRUE), silent = TRUE), add = TRUE)

  # let it resolve
  while (bgjm_status(job_id) == "running") Sys.sleep(0.05)
  expect_equal(bgjm_status(job_id), "error")
  expect_error(bgjm_collect(job_id), "error")
})

test_that("bgjm_result returns the bare result and bgjm_unresolved is non-blocking", {
  skip_if_no_mirai()
  output_base <- file.path(tempdir(), sprintf("bgjm-result-%d", Sys.getpid()))

  meta <- franzpak:::.bgjm_new_job("result-test", output_base)
  job_id <- franzpak:::.bgjm_submit(meta, function() {
    Sys.sleep(0.2)
    list(a = 1, b = 2)
  }, list())
  on.exit(try(bgjm_remove(job_id, delete_dir = TRUE), silent = TRUE), add = TRUE)

  # non-blocking check returns a single logical
  u <- bgjm_unresolved(job_id)
  expect_type(u, "logical")
  expect_length(u, 1)

  # bgjm_result blocks and returns the bare object (no metadata wrapper)
  res <- bgjm_result(job_id)
  expect_equal(res, list(a = 1, b = 2))

  # once resolved, unresolved() is FALSE
  expect_false(bgjm_unresolved(job_id))
})

test_that("bgjm_collect auto_remove drops the job from the registry", {
  skip_if_no_mirai()
  output_base <- file.path(tempdir(), sprintf("bgjm-autorm-%d", Sys.getpid()))

  meta <- franzpak:::.bgjm_new_job("autorm-test", output_base)
  job_id <- franzpak:::.bgjm_submit(meta, function() 42, list())

  collected <- bgjm_collect(job_id, auto_remove = TRUE)
  expect_equal(collected$result, 42)
  expect_error(bgjm_status(job_id), "Unknown job_id")
})

test_that("bgjm_kill terminates a running job", {
  skip_if_no_mirai()
  output_base <- file.path(tempdir(), sprintf("bgjm-kill-%d", Sys.getpid()))

  meta <- franzpak:::.bgjm_new_job("kill-test", output_base)
  job_id <- franzpak:::.bgjm_submit(
    meta, function() {
      Sys.sleep(30)
      "should not reach here"
    }, list()
  )
  on.exit(try(bgjm_remove(job_id, delete_dir = TRUE), silent = TRUE), add = TRUE)

  Sys.sleep(0.5)
  bgjm_kill(job_id)

  for (i in 1:100) {
    if (bgjm_status(job_id) != "running") break
    Sys.sleep(0.1)
  }
  expect_equal(bgjm_status(job_id), "error")
})

test_that("bgjm_start_lavaan returns a fitted lavaan object", {
  skip_if_no_mirai()
  skip_if_not_installed("lavaan")
  output_base <- file.path(tempdir(), sprintf("bgjm-lavaan-%d", Sys.getpid()))

  model_syntax <- "
    performance =~ mpg + hp + qsec
    size =~ wt + disp
  "
  job_id <- bgjm_start_lavaan(
    model_syntax = model_syntax,
    data = mtcars,
    lavaan_fun = "cfa",
    job_name = "lavaan-cfa-test",
    output_base = output_base
  )
  on.exit(try(bgjm_remove(job_id, delete_dir = TRUE), silent = TRUE), add = TRUE)

  collected <- bgjm_collect(job_id)
  expect_s4_class(collected$result, "lavaan")
  expect_true(lavaan::lavInspect(collected$result, "converged"))
})

test_that("bgjm_start_blavaan returns a fitted blavaan object", {
  skip_if_no_mirai()
  skip_if_not_installed("blavaan")
  skip_if_not_installed("lavaan")
  skip_on_cran()
  output_base <- file.path(tempdir(), sprintf("bgjm-blavaan-%d", Sys.getpid()))

  # Classic Bollen (1989) political democracy model
  model_syntax <- "
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem65 =~ y5 + y6 + y7 + y8
    dem60 ~ ind60
    dem65 ~ ind60 + dem60
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8
  "
  job_id <- bgjm_start_blavaan(
    model_syntax = model_syntax,
    data = lavaan::PoliticalDemocracy,
    blavaan_fun = "bsem",
    n.chains = 2, burnin = 100, sample = 100,
    job_name = "blavaan-bsem-test",
    output_base = output_base
  )
  on.exit(try(bgjm_remove(job_id, delete_dir = TRUE), silent = TRUE), add = TRUE)

  fit <- bgjm_result(job_id)
  expect_s4_class(fit, "blavaan")
})

test_that("bgjm_start_blavaan runs parallel MCMC chains inside a job", {
  skip_if_no_mirai()
  skip_if_not_installed("blavaan")
  skip_if_not_installed("lavaan")
  skip_if_not_installed("rstan")
  skip_on_cran()
  output_base <- file.path(tempdir(), sprintf("bgjm-blavaan-par-%d", Sys.getpid()))

  # Parallel chains use parallel::mclapply (fork) on Unix; this guards that
  # forking from inside a mirai daemon still produces a valid fit.
  job_id <- bgjm_start_blavaan(
    model_syntax = "
      ind60 =~ x1 + x2 + x3
      dem60 =~ y1 + y2 + y3 + y4
      dem60 ~ ind60
    ",
    data = lavaan::PoliticalDemocracy,
    blavaan_fun = "bsem",
    n.chains = 2, burnin = 100, sample = 100,
    bcontrol = list(cores = 2),
    job_name = "blavaan-parallel-test",
    output_base = output_base
  )
  on.exit(try(bgjm_remove(job_id, delete_dir = TRUE), silent = TRUE), add = TRUE)

  fit <- bgjm_result(job_id)
  expect_s4_class(fit, "blavaan")
})

test_that("bgjm_start_tidylpa works with the mclust backend", {
  skip_if_no_mirai()
  skip_if_not_installed("tidyLPA")
  skip_if_not_installed("mclust")
  output_base <- file.path(tempdir(), sprintf("bgjm-tidylpa-%d", Sys.getpid()))

  job_id <- bgjm_start_tidylpa(
    data = iris[, 1:4],
    n_profiles = 3,
    models = 1,
    package = "mclust",
    job_name = "tidylpa-mclust-test",
    output_base = output_base
  )
  on.exit(try(bgjm_remove(job_id, delete_dir = TRUE), silent = TRUE), add = TRUE)

  collected <- bgjm_collect(job_id)
  expect_type(collected$result, "list")
  expect_true(any(grepl("class_3", names(collected$result))))
})

test_that("Mplus fixture can be generated without running Mplus", {
  skip_if_not_installed("MplusAutomation")
  dir <- file.path(tempdir(), sprintf("bgjm-mplus-fixture-%d", Sys.getpid()))
  make_mplus_fixture(dir)
  expect_true(file.exists(file.path(dir, "fixture.inp")))
  expect_true(file.exists(file.path(dir, "fixture.dat")))
  unlink(dir, recursive = TRUE, force = TRUE)
})

test_that("bgjm_start_mplus runs an input and captures .out artifacts", {
  skip_if_no_mirai()
  skip_if_not_installed("MplusAutomation")
  if (!mplus_available()) skip("Mplus executable not available")

  fixture_dir <- file.path(tempdir(), sprintf("bgjm-mplus-fix-%d", Sys.getpid()))
  make_mplus_fixture(fixture_dir)
  on.exit(unlink(fixture_dir, recursive = TRUE, force = TRUE), add = TRUE)

  output_base <- file.path(tempdir(), sprintf("bgjm-mplus-%d", Sys.getpid()))
  job_id <- bgjm_start_mplus(
    target = fixture_dir,
    job_name = "mplus-test",
    output_base = output_base
  )
  on.exit(try(bgjm_remove(job_id, delete_dir = TRUE), silent = TRUE), add = TRUE)

  collected <- bgjm_collect(job_id)

  arts <- basename(collected$artifact_files)
  expect_true(any(grepl("\\.out$", arts)))
  expect_true(any(grepl("mplus_run\\.log$", arts)))
  # Mplus console output routed through R was captured to stdout log
  expect_true(length(bgjm_read_stdout(job_id)) > 0)
  # readModels() result is returned
  expect_true(is.list(collected$result))
})
