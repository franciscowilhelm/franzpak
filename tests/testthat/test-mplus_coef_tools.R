find_extdata <- function(filename) {
  path <- system.file("extdata", filename, package = "franzpak")
  if (nzchar(path)) return(path)
  normalizePath(file.path("inst", "extdata", filename), mustWork = TRUE)
}

# Helper: extract the underlying tibble from either a gt_tbl or plain tibble
table_data <- function(tbl) {
  if (inherits(tbl, "gt_tbl")) tbl[["_data"]] else tbl
}

# --- sep_label_mplus (internal) ---

test_that("sep_label_mplus parses <- labels into DV and IV columns", {
  example_coefs <- tibble::tibble(
    Label = c(" F4<-F3", " F3<-F1", " F3<-F2"),
    est   = c(0.473, 0.563, 0.790),
    se    = c(0.057, 0.070, 0.086),
    pval  = c(0, 0, 0)
  )

  result <- franzpak:::sep_label_mplus(example_coefs)

  expect_equal(result$DV, c(" F4", " F3", " F3"))
  expect_equal(result$IV, c("F3", "F1", "F2"))
  expect_equal(nrow(result), 3)
  expect_true(all(c("Label", "est", "se", "pval", "DV", "IV") %in% names(result)))
})

test_that("sep_label_mplus sets DV and IV to NA when no <- separator", {
  example_coefs <- tibble::tibble(
    Label = c(" F1", " Y1", " Y2"),
    est   = c(0.884, 0.011, 0.047),
    se    = c(0.122, 0.062, 0.064),
    pval  = c(0, 0.855, 0.460)
  )

  result <- franzpak:::sep_label_mplus(example_coefs)

  expect_true(all(is.na(result$DV)))
  expect_true(all(is.na(result$IV)))
})

test_that("sep_label_mplus preserves est, se, and pval columns unchanged", {
  example_coefs <- tibble::tibble(
    Label = c(" F4<-F3", " F3<-F1"),
    est   = c(0.473, 0.563),
    se    = c(0.057, 0.070),
    pval  = c(0.001, 0.002)
  )

  result <- franzpak:::sep_label_mplus(example_coefs)

  expect_equal(result$est, example_coefs$est)
  expect_equal(result$se,  example_coefs$se)
  expect_equal(result$pval, example_coefs$pval)
})

# --- coef_wrapper: auto-detection and conflict warnings ---

test_that("coef_wrapper auto-detects ML estimator without warning or message", {
  skip_if_not_installed("MplusAutomation")

  m <- MplusAutomation::readModels(find_extdata("ex5.11.out"), quiet = TRUE)
  expect_no_warning(expect_no_message(coef_wrapper(m)))
})

test_that("coef_wrapper auto-detects BAYES and emits one-tailed message, no warning", {
  skip_if_not_installed("MplusAutomation")

  m <- MplusAutomation::readModels(find_extdata("ex5.11_BAYES.out"), quiet = TRUE)
  expect_no_warning(
    expect_message(coef_wrapper(m), regexp = "one-tailed")
  )
})

test_that("coef_wrapper warns when bayes = TRUE on an ML model", {
  skip_if_not_installed("MplusAutomation")

  m <- MplusAutomation::readModels(find_extdata("ex5.11.out"), quiet = TRUE)
  expect_warning(
    coef_wrapper(m, bayes = TRUE),
    regexp = "does not appear to be BAYES"
  )
})

test_that("coef_wrapper warns when bayes = FALSE on a Bayes model", {
  skip_if_not_installed("MplusAutomation")

  m <- MplusAutomation::readModels(find_extdata("ex5.11_BAYES.out"), quiet = TRUE)
  suppressMessages(
    expect_warning(coef_wrapper(m, bayes = FALSE), regexp = "estimated with BAYES")
  )
})

test_that("coef_wrapper no warning when bayes = TRUE on a Bayes model", {
  skip_if_not_installed("MplusAutomation")

  m <- MplusAutomation::readModels(find_extdata("ex5.11_BAYES.out"), quiet = TRUE)
  suppressMessages(
    expect_no_warning(coef_wrapper(m, bayes = TRUE))
  )
})

# --- coef_wrapper: ML coefficients ---

test_that("coef_wrapper returns regression coefficients with DV/IV columns for ex5.11", {
  skip_if_not_installed("MplusAutomation")

  m <- MplusAutomation::readModels(find_extdata("ex5.11.out"), quiet = TRUE)
  result <- coef_wrapper(m)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("Label", "est", "se", "pval", "DV", "IV") %in% names(result)))
  expect_equal(nrow(result), 3)
  expect_equal(trimws(result$DV), c("F4", "F3", "F3"))
  expect_equal(trimws(result$IV), c("F3", "F1", "F2"))
  expect_equal(result$est, c(0.473, 0.563, 0.790), tolerance = 1e-3)
})

test_that("coef_wrapper with addci = TRUE appends LowerCI and UpperCI", {
  skip_if_not_installed("MplusAutomation")

  m <- MplusAutomation::readModels(find_extdata("ex5.11.out"), quiet = TRUE)
  result <- coef_wrapper(m, addci = TRUE)

  expect_true("LowerCI" %in% names(result))
  expect_true("UpperCI" %in% names(result))
  expect_equal(result$LowerCI, c(0.361, 0.425, 0.622), tolerance = 1e-3)
  expect_equal(result$UpperCI, c(0.585, 0.700, 0.958), tolerance = 1e-3)
})

test_that("coef_wrapper label_replace renames DV and IV labels", {
  skip_if_not_installed("MplusAutomation")

  m <- MplusAutomation::readModels(find_extdata("ex5.11.out"), quiet = TRUE)
  replacements <- c("F4" = "Outcome", "F3" = "Mediator", "F1" = "Pred1", "F2" = "Pred2")
  result <- coef_wrapper(m, label_replace = replacements)

  expect_equal(trimws(result$DV), c("Outcome", "Mediator", "Mediator"))
  expect_equal(trimws(result$IV), c("Mediator", "Pred1", "Pred2"))
  expect_false(any(grepl("F3|F4", result$DV), na.rm = TRUE))
})

test_that("coef_wrapper does not modify p-values regardless of bayes setting", {
  skip_if_not_installed("MplusAutomation")

  m <- MplusAutomation::readModels(find_extdata("ex5.11.out"), quiet = TRUE)
  result_default <- coef_wrapper(m)
  result_bayes   <- suppressWarnings(coef_wrapper(m, bayes = TRUE))

  expect_equal(result_bayes$pval, result_default$pval, tolerance = 1e-10)
})

# --- coef_wrapper: Bayes coefficients (auto-detected) ---

test_that("coef_wrapper auto-detects BAYES and returns correct estimates", {
  skip_if_not_installed("MplusAutomation")

  m <- MplusAutomation::readModels(find_extdata("ex5.11_BAYES.out"), quiet = TRUE)
  result <- suppressMessages(coef_wrapper(m))

  expect_equal(nrow(result), 3)
  expect_equal(trimws(result$DV), c("F4", "F3", "F3"))
  expect_equal(trimws(result$IV), c("F3", "F1", "F2"))
  expect_equal(result$est, c(0.466, 0.560, 0.799), tolerance = 1e-3)
  # p-values kept as-is (one-tailed, not doubled)
  expect_equal(result$pval, c(0, 0, 0))
})

test_that("coef_wrapper Bayes with addci returns credibility intervals", {
  skip_if_not_installed("MplusAutomation")

  m <- MplusAutomation::readModels(find_extdata("ex5.11_BAYES.out"), quiet = TRUE)
  result <- suppressMessages(coef_wrapper(m, addci = TRUE))

  expect_true(all(c("LowerCI", "UpperCI") %in% names(result)))
  expect_equal(result$LowerCI, c(0.353, 0.428, 0.635), tolerance = 1e-3)
  expect_equal(result$UpperCI, c(0.582, 0.715, 0.984), tolerance = 1e-3)
})

# --- coef_table_mplus: ML (auto-detected) ---

test_that("coef_table_mplus auto-detects ML and produces Est./SE table", {
  skip_if_not_installed("MplusAutomation")

  m <- MplusAutomation::readModels(find_extdata("ex5.11.out"), quiet = TRUE)
  d <- expect_no_warning(table_data(coef_table_mplus(m)))

  expect_equal(nrow(d), 3)
  expect_true(all(c("est_col__F4", "se_col__F4", "est_col__F3", "se_col__F3") %in% names(d)))
  expect_false(any(grepl("^ll_col|^ul_col", names(d))))

  expect_true(grepl("0\\.473\\*", d$est_col__F4[d$IV == "F3"]))
  expect_true(grepl("0\\.563\\*", d$est_col__F3[d$IV == "F1"]))
  expect_true(grepl("0\\.790\\*", d$est_col__F3[d$IV == "F2"]))
})

test_that("coef_table_mplus ML with addci adds LL and UL columns", {
  skip_if_not_installed("MplusAutomation")

  m <- MplusAutomation::readModels(find_extdata("ex5.11.out"), quiet = TRUE)
  d <- table_data(coef_table_mplus(m, addci = TRUE))

  expect_true(all(c("ll_col__F4", "ul_col__F4", "ll_col__F3", "ul_col__F3") %in% names(d)))
  expect_equal(as.numeric(d$ll_col__F4[d$IV == "F3"]), 0.361, tolerance = 1e-3)
  expect_equal(as.numeric(d$ul_col__F4[d$IV == "F3"]), 0.585, tolerance = 1e-3)
})

# --- coef_table_mplus: Bayes (auto-detected) ---

test_that("coef_table_mplus auto-detects BAYES and uses CrI columns", {
  skip_if_not_installed("MplusAutomation")

  m <- MplusAutomation::readModels(find_extdata("ex5.11_BAYES.out"), quiet = TRUE)
  d <- expect_no_warning(table_data(coef_table_mplus(m)))

  expect_equal(nrow(d), 3)
  expect_true(all(c("est_col__F4", "ll_col__F4", "ul_col__F4",
                    "est_col__F3", "ll_col__F3", "ul_col__F3") %in% names(d)))
  expect_false(any(grepl("^se_col", names(d))))
})

test_that("coef_table_mplus Bayes CrI values and significance stars are correct", {
  skip_if_not_installed("MplusAutomation")

  m <- MplusAutomation::readModels(find_extdata("ex5.11_BAYES.out"), quiet = TRUE)
  d <- table_data(coef_table_mplus(m))

  f4_row <- d[d$IV == "F3", ]
  expect_equal(as.numeric(f4_row$ll_col__F4), 0.353, tolerance = 1e-3)
  expect_equal(as.numeric(f4_row$ul_col__F4), 0.582, tolerance = 1e-3)
  expect_true(grepl("\\*", f4_row$est_col__F4))
  expect_true(grepl("\\*", d$est_col__F3[d$IV == "F1"]))
  expect_true(grepl("\\*", d$est_col__F3[d$IV == "F2"]))
})
