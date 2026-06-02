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

# --- coef_wrapper: type argument ---

test_that("coef_wrapper type = stdyx returns different estimates than un", {
  skip_if_not_installed("MplusAutomation")

  m      <- MplusAutomation::readModels(find_extdata("ex5.11.out"), quiet = TRUE)
  un     <- coef_wrapper(m)
  stdyx  <- coef_wrapper(m, type = "stdyx")

  expect_false(isTRUE(all.equal(un$est, stdyx$est)))
  expect_equal(nrow(stdyx), 3)
  expect_true(all(c("DV", "IV", "est", "se", "pval") %in% names(stdyx)))
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

test_that("coef_table_mplus ML with display_type est_ci adds LL/UL and drops SE", {
  skip_if_not_installed("MplusAutomation")

  m <- MplusAutomation::readModels(find_extdata("ex5.11.out"), quiet = TRUE)
  d <- table_data(coef_table_mplus(m, display_type = "est_ci"))

  expect_true(all(c("ll_col__F4", "ul_col__F4", "ll_col__F3", "ul_col__F3") %in% names(d)))
  expect_false(any(grepl("^se_col", names(d))))
  expect_equal(as.numeric(d$ll_col__F4[d$IV == "F3"]), 0.361, tolerance = 1e-3)
  expect_equal(as.numeric(d$ul_col__F4[d$IV == "F3"]), 0.585, tolerance = 1e-3)
})

test_that("coef_table_mplus Bayes with display_type est_se uses SE columns, no CrI", {
  skip_if_not_installed("MplusAutomation")

  m <- MplusAutomation::readModels(find_extdata("ex5.11_BAYES.out"), quiet = TRUE)
  d <- suppressMessages(table_data(coef_table_mplus(m, display_type = "est_se")))

  expect_true(all(c("est_col__F4", "se_col__F4", "est_col__F3", "se_col__F3") %in% names(d)))
  expect_false(any(grepl("^ll_col|^ul_col", names(d))))
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

# --- helpers: random-slope detection and two-level detection ---

test_that("mplus_random_slopes parses the | declaration for ex9.2c", {
  skip_if_not_installed("MplusAutomation")

  m  <- MplusAutomation::readModels(find_extdata("ex9.2c.out"), quiet = TRUE)
  rs <- franzpak:::mplus_random_slopes(m)

  expect_equal(rs$slope, "S")
  expect_equal(rs$definition, "Y ON X")
})

test_that("mplus_random_slopes returns an empty tibble for single-level models", {
  skip_if_not_installed("MplusAutomation")

  m  <- MplusAutomation::readModels(find_extdata("ex5.11.out"), quiet = TRUE)
  rs <- franzpak:::mplus_random_slopes(m)

  expect_equal(nrow(rs), 0)
})

test_that("is_mplus_twolevel distinguishes two-level from single-level models", {
  skip_if_not_installed("MplusAutomation")

  m2 <- MplusAutomation::readModels(find_extdata("ex9.2c.out"), quiet = TRUE)
  m1 <- MplusAutomation::readModels(find_extdata("ex5.11.out"), quiet = TRUE)

  expect_true(franzpak:::is_mplus_twolevel(m2))
  expect_false(franzpak:::is_mplus_twolevel(m1))
})

# --- coef_wrapper: two-level annotations ---

test_that("coef_wrapper adds level / random_slope columns for two-level models", {
  skip_if_not_installed("MplusAutomation")

  m <- MplusAutomation::readModels(find_extdata("ex9.2c.out"), quiet = TRUE)
  r <- coef_wrapper(m, params = c("regression", "expectation", "variability"))

  expect_true(all(c("level", "random_slope", "within_avg_effect",
                    "random_effect", "variance", "display_level") %in% names(r)))

  # The mean/intercept of the random slope is re-expressed as the within-person
  # effect it represents: s | y ON x  ->  DV = "Y", IV = "X", in the Within section.
  rsm <- r[r$within_avg_effect, ]
  expect_equal(nrow(rsm), 1)
  expect_equal(trimws(rsm$DV), "Y")
  expect_equal(trimws(rsm$IV), "X")
  expect_equal(rsm$display_level, "Within")
  expect_equal(rsm$est, 1.017, tolerance = 1e-3)

  # Cross-level regressions on the slope (s ON w) move to the Random effects
  # section, re-expressed under the slope outcome Y (keeping the between predictor).
  s_on_w <- r[r$random_effect & trimws(r$IV) == "W", ]
  expect_equal(nrow(s_on_w), 1)
  expect_equal(trimws(s_on_w$DV), "Y")
  expect_equal(s_on_w$display_level, "Random effects")
  expect_equal(s_on_w$est, 0.569, tolerance = 1e-3)

  # The random-slope (residual) variance is in the Random effects section.
  rvar <- r[r$random_effect & r$variance, ]
  expect_equal(nrow(rvar), 1)
  expect_equal(trimws(rvar$DV), "Y")
  expect_equal(trimws(rvar$IV), "Residual variance")
  expect_equal(rvar$est, 0.368, tolerance = 1e-3)

  # Outcome residual variances are *not* flagged as random effects.
  y_var <- r[r$variance & !r$random_effect, ]
  expect_true(nrow(y_var) >= 1)
})

test_that("coef_wrapper omits multilevel columns for single-level models", {
  skip_if_not_installed("MplusAutomation")

  m <- MplusAutomation::readModels(find_extdata("ex5.11.out"), quiet = TRUE)
  r <- coef_wrapper(m)

  expect_false(any(c("level", "random_slope", "within_avg_effect", "display_level") %in% names(r)))
  # The original coefficient columns are still present.
  expect_true(all(c("Label", "est", "se", "pval", "DV", "IV") %in% names(r)))
})

test_that("coef_wrapper recovers credibility intervals for a two-level Bayes model", {
  skip_if_not_installed("MplusAutomation")

  m <- MplusAutomation::readModels(find_extdata("ex9.2c_BAYES.out"), quiet = TRUE)
  r <- suppressMessages(
    coef_wrapper(m, params = c("regression", "expectation"), addci = TRUE)
  )

  expect_true(all(c("LowerCI", "UpperCI") %in% names(r)))
  # The random-slope mean (remapped to the within X -> Y effect) carries its CrI.
  rsm <- r[r$within_avg_effect, ]
  expect_equal(nrow(rsm), 1)
  expect_equal(trimws(rsm$DV), "Y")
  expect_equal(trimws(rsm$IV), "X")
  expect_equal(rsm$LowerCI, 0.862, tolerance = 1e-3)
  expect_equal(rsm$UpperCI, 1.166, tolerance = 1e-3)
})

test_that("mplus_confint_safe falls back to inline intervals when no CINTERVAL table", {
  skip_if_not_installed("MplusAutomation")

  m <- MplusAutomation::readModels(find_extdata("ex9.2c_BAYES.out"), quiet = TRUE)

  # Reference values come from the (present) CINTERVAL table via confint().
  ref <- franzpak:::mplus_confint_safe(m, params = c("regression", "expectation"), type = "un")

  # Drop the CINTERVAL table so confint() can no longer succeed.
  m$parameters$ci.unstandardized <- NULL
  expect_error(confint(m, params = c("regression", "expectation")))

  fb <- franzpak:::mplus_confint_safe(m, params = c("regression", "expectation"), type = "un")

  # Fallback reconstructs the same labels and interval bounds from the
  # inline lower_2.5ci/upper_2.5ci columns of the Bayes estimate table.
  expect_setequal(fb$Label, ref$Label)
  ref <- ref[match(fb$Label, ref$Label), ]
  expect_equal(fb$LowerCI, ref$LowerCI, tolerance = 1e-6)
  expect_equal(fb$UpperCI, ref$UpperCI, tolerance = 1e-6)
})

# --- coef_table_mplus: two-level Within/Between sections ---

test_that("coef_table_mplus splits two-level model into Within/Between sections", {
  skip_if_not_installed("MplusAutomation")

  m <- MplusAutomation::readModels(find_extdata("ex9.2c.out"), quiet = TRUE)
  tbl <- coef_table_mplus(m)
  d   <- table_data(tbl)

  # Plain tibble carries a leading `level` column distinguishing the sections.
  expect_true("level" %in% names(d))
  expect_setequal(unique(d$level), c("Within", "Between", "Random effects"))

  # There is no separate column for the random slope S; everything is under Y.
  expect_false(any(grepl("__S$", names(d))))

  # The avg. within effect of X on Y appears as predictor X (not "Intercepts")
  # under outcome Y, in the Within section, marked with the random-slope letter.
  within_x <- d[d$level == "Within" & d$IV == "X", ]
  expect_equal(nrow(within_x), 1)
  expect_true(grepl("1\\.017\\*", within_x$est_col__Y))
  expect_true(grepl("\u1d43", within_x$est_col__Y, fixed = TRUE))
  expect_false("Intercepts" %in% d$IV[d$level == "Within"])

  # Within rows come first; Random effects last.
  expect_equal(d$level[1], "Within")
  expect_equal(d$level[nrow(d)], "Random effects")

  # The between-person effect of X on Y is a distinct (fixed) coefficient in the
  # Between section, with no random-slope marker.
  between_x <- d[d$level == "Between" & d$IV == "X", ]
  expect_true(grepl("1\\.024\\*", between_x$est_col__Y))
  expect_false(grepl("\u1d43", between_x$est_col__Y, fixed = TRUE))

  # Between-person effects on Y are in the Between section.
  between_w <- d[d$level == "Between" & d$IV == "W", ]
  expect_true(grepl("1\\.186\\*", between_w$est_col__Y))

  # Random effects section: cross-level predictor W of the slope, and the
  # slope's (residual) variance.
  re_w <- d[d$level == "Random effects" & d$IV == "W", ]
  expect_true(grepl("0\\.569\\*", re_w$est_col__Y))
  re_var <- d[d$level == "Random effects" & d$IV == "Residual variance", ]
  expect_equal(nrow(re_var), 1)
  expect_true(grepl("0\\.368\\*", re_var$est_col__Y))
})

test_that("coef_table_mplus footnote distinguishes random-slope means from fixed effects", {
  skip_if_not_installed("MplusAutomation")
  skip_if_not_installed("gt")

  m   <- MplusAutomation::readModels(find_extdata("ex9.2c.out"), quiet = TRUE)
  tbl <- coef_table_mplus(m)
  html <- as.character(gt::as_raw_html(tbl))

  expect_true(grepl("Random-slope mean/intercept", html))
  expect_true(grepl("fixed effects", html))
})

test_that("coef_table_mplus two-level gt output has Within/Between/Random effects row groups", {
  skip_if_not_installed("MplusAutomation")
  skip_if_not_installed("gt")

  m   <- MplusAutomation::readModels(find_extdata("ex9.2c.out"), quiet = TRUE)
  tbl <- coef_table_mplus(m)

  expect_s3_class(tbl, "gt_tbl")
  expect_setequal(unique(tbl[["_row_groups"]]), c("Within", "Between", "Random effects"))

  # The Random effects note is present.
  html <- as.character(gt::as_raw_html(tbl))
  expect_true(grepl("Random effects:", html))
  expect_true(grepl("cross-level", html))
})

test_that("coef_table_mplus single-level output has no level column", {
  skip_if_not_installed("MplusAutomation")

  m <- MplusAutomation::readModels(find_extdata("ex5.11.out"), quiet = TRUE)
  d <- table_data(coef_table_mplus(m))

  expect_false("level" %in% names(d))
})
