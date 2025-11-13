# Helper function to wait for job completion
wait_for_job <- function(job_id, max_iterations = 50, timeout_ms = 200) {
  status <- NULL
  for (i in seq_len(max_iterations)) {
    status <- bgjm_status(job_id)
    if (status != "running") {
      break
    }
    bgjm_poll(timeout_ms = timeout_ms)
  }
  status
}

# Helper function to check if Mplus is available
mplus_available <- function() {
  if (!requireNamespace("MplusAutomation", quietly = TRUE)) {
    return(FALSE)
  }

  # Try to detect Mplus executable
  mplus_path <- tryCatch({
    MplusAutomation::detectMplus()
    TRUE
  }, error = function(e) {
    FALSE
  })

  return(mplus_path)
}

test_that("background job lifecycle mirrors usage example", {
  output_base <- file.path(
    tempdir(),
    sprintf("bgjm-test-%d-%d", Sys.getpid(), as.integer(runif(1, 1, 1e6)))
  )

  fun_expr <- substitute(function(.n) {
    writeLines("background side effect", paste0("artifact-", .n, ".txt"))
    sum(seq_len(.n))
  })

  job_id <- franzpak:::`.bgjm_start_job`(
    fun_expr = fun_expr,
    args = list(.n = 5),
    job_name = "callr-wrapper-test",
    output_base = output_base,
    return_strategy = "inline"
  )

  on.exit(
    try(bgjm_remove(job_id, delete_dir = TRUE), silent = TRUE),
    add = TRUE
  )

  status <- wait_for_job(job_id)
  expect_equal(status, "finished")

  listed <- bgjm_list()
  expect_true(job_id %in% listed$id)

  collected <- bgjm_collect(job_id)
  expect_equal(collected$result, sum(seq_len(5)))
  expect_null(collected$result_rds)
  expect_length(collected$artifact_files, 1)
  expect_true(all(file.exists(collected$artifact_files)))

  expect_type(bgjm_read_stdout(job_id), "character")
  expect_type(bgjm_read_stderr(job_id), "character")
})

test_that("bgjm_start_lavaan works with inline return strategy", {
  skip_if_not_installed("lavaan")

  output_base <- file.path(tempdir(), sprintf("bgjm-lavaan-test-%d", Sys.getpid()))

  # Simple CFA model using mtcars data
  model_syntax <- "
    performance =~ mpg + hp + qsec
    size =~ wt + disp
  "

  job_id <- bgjm_start_lavaan(
    model_syntax = model_syntax,
    data = mtcars,
    lavaan_fun = "cfa",
    job_name = "test-lavaan-cfa",
    output_base = output_base,
    return_strategy = "inline",
    pass_data_as = "object"
  )

  on.exit(try(bgjm_remove(job_id, delete_dir = TRUE), silent = TRUE), add = TRUE)

  status <- wait_for_job(job_id, max_iterations = 100, timeout_ms = 500)
  expect_equal(status, "finished")

  collected <- bgjm_collect(job_id)
  expect_s4_class(collected$result, "lavaan")
  expect_null(collected$result_rds)

  # Check that lavaan object has expected structure
  expect_true(lavaan::lavInspect(collected$result, "converged"))
})

test_that("bgjm_start_lavaan works with RDS return strategy", {
  skip_if_not_installed("lavaan")

  output_base <- file.path(tempdir(), sprintf("bgjm-lavaan-rds-test-%d", Sys.getpid()))

  model_syntax <- "mpg ~ wt + hp"

  job_id <- bgjm_start_lavaan(
    model_syntax = model_syntax,
    data = mtcars,
    lavaan_fun = "sem",
    output_base = output_base,
    return_strategy = "rds",
    pass_data_as = "object"
  )

  on.exit(try(bgjm_remove(job_id, delete_dir = TRUE), silent = TRUE), add = TRUE)

  status <- wait_for_job(job_id, max_iterations = 100, timeout_ms = 500)
  expect_equal(status, "finished")

  collected <- bgjm_collect(job_id, read_rds = TRUE)
  expect_s4_class(collected$result, "lavaan")
  expect_true(file.exists(collected$result_rds))
})

test_that("bgjm_start_lavaan works with pass_data_as = 'rds'", {
  skip_if_not_installed("lavaan")

  output_base <- file.path(tempdir(), sprintf("bgjm-lavaan-datards-test-%d", Sys.getpid()))

  model_syntax <- "mpg ~ wt + hp + cyl"

  job_id <- bgjm_start_lavaan(
    model_syntax = model_syntax,
    data = mtcars,
    lavaan_fun = "sem",
    output_base = output_base,
    return_strategy = "inline",
    pass_data_as = "rds"
  )

  on.exit(try(bgjm_remove(job_id, delete_dir = TRUE), silent = TRUE), add = TRUE)

  status <- wait_for_job(job_id, max_iterations = 100, timeout_ms = 500)
  expect_equal(status, "finished")

  collected <- bgjm_collect(job_id)
  expect_s4_class(collected$result, "lavaan")
})

test_that("bgjm_start_tidylpa works with mclust package and inline return", {
  skip_if_not_installed("tidyLPA")
  skip_if_not_installed("mclust")

  output_base <- file.path(tempdir(), sprintf("bgjm-tidylpa-test-%d", Sys.getpid()))

  # Use iris data (numeric columns only)
  iris_numeric <- iris[, 1:4]

  job_id <- bgjm_start_tidylpa(
    data = iris_numeric,
    n_profiles = 3,
    models = 1,
    package = "mclust",
    job_name = "test-tidylpa-mclust",
    output_base = output_base,
    return_strategy = "inline",
    pass_data_as = "object"
  )

  on.exit(try(bgjm_remove(job_id, delete_dir = TRUE), silent = TRUE), add = TRUE)

  status <- wait_for_job(job_id, max_iterations = 100, timeout_ms = 500)
  expect_equal(status, "finished")

  collected <- bgjm_collect(job_id)

  # Check that tidyLPA result has expected structure
  expect_type(collected$result, "list")
  expect_true(length(collected$result) > 0)
  # tidyLPA returns results named like "model_1_class_3"
  expect_true(any(grepl("class_3", names(collected$result))))
})

test_that("bgjm_start_tidylpa works with RDS return strategy", {
  skip_if_not_installed("tidyLPA")
  skip_if_not_installed("mclust")

  output_base <- file.path(tempdir(), sprintf("bgjm-tidylpa-rds-test-%d", Sys.getpid()))

  iris_numeric <- iris[, 1:4]

  job_id <- bgjm_start_tidylpa(
    data = iris_numeric,
    n_profiles = 2,
    models = 1,
    package = "mclust",
    output_base = output_base,
    return_strategy = "rds",
    pass_data_as = "object"
  )

  on.exit(try(bgjm_remove(job_id, delete_dir = TRUE), silent = TRUE), add = TRUE)

  status <- wait_for_job(job_id, max_iterations = 100, timeout_ms = 500)
  expect_equal(status, "finished")

  collected <- bgjm_collect(job_id, read_rds = TRUE)

  expect_type(collected$result, "list")
  expect_true(file.exists(collected$result_rds))
})

test_that("bgjm_start_tidylpa works with pass_data_as = 'rds'", {
  skip_if_not_installed("tidyLPA")
  skip_if_not_installed("mclust")

  output_base <- file.path(tempdir(), sprintf("bgjm-tidylpa-datards-test-%d", Sys.getpid()))

  iris_numeric <- iris[, 1:4]

  job_id <- bgjm_start_tidylpa(
    data = iris_numeric,
    n_profiles = 2,
    models = 1,
    package = "mclust",
    output_base = output_base,
    return_strategy = "inline",
    pass_data_as = "rds"
  )

  on.exit(try(bgjm_remove(job_id, delete_dir = TRUE), silent = TRUE), add = TRUE)

  status <- wait_for_job(job_id, max_iterations = 100, timeout_ms = 500)
  expect_equal(status, "finished")

  collected <- bgjm_collect(job_id)

  expect_type(collected$result, "list")
})

test_that("bgjm_start_tidylpa works with Mplus package", {
  skip_if_not_installed("tidyLPA")
  skip_if_not_installed("MplusAutomation")

  if (!mplus_available()) {
    skip("Mplus executable not available")
  }

  output_base <- file.path(tempdir(), sprintf("bgjm-tidylpa-mplus-test-%d", Sys.getpid()))

  # Use iris data (numeric columns only)
  # Mplus requires variable names to be alphanumeric + underscore (no dots)
  iris_numeric <- iris[, 1:4]
  names(iris_numeric) <- c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width")

  job_id <- bgjm_start_tidylpa(
    data = iris_numeric,
    n_profiles = 2,
    models = 1,
    package = "Mplus",
    job_name = "test-tidylpa-mplus",
    output_base = output_base,
    return_strategy = "inline",
    pass_data_as = "object"
  )

  on.exit(try(bgjm_remove(job_id, delete_dir = TRUE), silent = TRUE), add = TRUE)

  # Mplus jobs may take longer
  status <- wait_for_job(job_id, max_iterations = 200, timeout_ms = 1000)
  expect_equal(status, "finished")

  collected <- bgjm_collect(job_id)

  # Check that tidyLPA with Mplus backend returns expected structure
  expect_type(collected$result, "list")
  expect_true(length(collected$result) > 0)

  # Verify that tidyLPA results have the expected naming pattern
  result_names <- names(collected$result)
  expect_true(any(grepl("class_2", result_names)))
})

test_that("bgjm_kill terminates a running job", {
  output_base <- file.path(tempdir(), sprintf("bgjm-kill-test-%d", Sys.getpid()))

  # Create a long-running job
  fun_expr <- substitute(function() {
    Sys.sleep(30)
    "should not reach here"
  })

  job_id <- franzpak:::`.bgjm_start_job`(
    fun_expr = fun_expr,
    args = list(),
    job_name = "test-kill",
    output_base = output_base,
    return_strategy = "inline"
  )

  on.exit(try(bgjm_remove(job_id, delete_dir = TRUE), silent = TRUE), add = TRUE)

  # Wait a bit to ensure job starts
  Sys.sleep(0.5)

  # Kill the job
  expect_true(bgjm_kill(job_id))

  # Wait for process to die
  Sys.sleep(1)

  status <- bgjm_status(job_id)
  expect_equal(status, "error")
})

test_that("bgjm_collect auto_remove removes job from registry", {
  output_base <- file.path(tempdir(), sprintf("bgjm-autoremove-test-%d", Sys.getpid()))

  fun_expr <- substitute(function() {
    42
  })

  job_id <- franzpak:::`.bgjm_start_job`(
    fun_expr = fun_expr,
    args = list(),
    job_name = "test-autoremove",
    output_base = output_base,
    return_strategy = "inline"
  )

  status <- wait_for_job(job_id)
  expect_equal(status, "finished")

  collected <- bgjm_collect(job_id, auto_remove = TRUE)
  expect_equal(collected$result, 42)

  # Job should no longer be in registry
  expect_error(bgjm_status(job_id), "Unknown job_id")
})

test_that("stdout and stderr are captured correctly", {
  output_base <- file.path(tempdir(), sprintf("bgjm-logs-test-%d", Sys.getpid()))

  fun_expr <- substitute(function() {
    message("This is a message")
    cat("This is stdout output\n")
    warning("This is a warning")
    return(123)
  })

  job_id <- franzpak:::`.bgjm_start_job`(
    fun_expr = fun_expr,
    args = list(),
    job_name = "test-logs",
    output_base = output_base,
    return_strategy = "inline"
  )

  on.exit(try(bgjm_remove(job_id, delete_dir = TRUE), silent = TRUE), add = TRUE)

  status <- wait_for_job(job_id)
  expect_equal(status, "finished")

  stdout_lines <- bgjm_read_stdout(job_id)
  stderr_lines <- bgjm_read_stderr(job_id)

  expect_type(stdout_lines, "character")
  expect_type(stderr_lines, "character")

  # Check that our outputs are captured
  expect_true(any(grepl("stdout output", stdout_lines)))
  expect_true(any(grepl("message|warning", stderr_lines, ignore.case = TRUE)))
})
