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

test_that("apply_label_replace resolves prefix-overlapping keys longest-first", {
  # A short key that is a prefix of a longer variable name must not corrupt the
  # longer name's label (the str_replace_all substring-bleed footgun). Both keys
  # are present; each variable must get its own label.
  repl <- c("M_PA" = "Positive affect", "M_PAW" = "Positive affect after work")
  expect_equal(
    apply_label_replace(c("M_PA", "M_PAW"), repl),
    c("Positive affect", "Positive affect after work")
  )
  # Order in the input vector must not matter.
  expect_equal(
    apply_label_replace(c("M_PAW", "M_PA"), rev(repl)),
    c("Positive affect after work", "Positive affect")
  )
  # NULL / empty pass through unchanged.
  expect_equal(apply_label_replace(c("a", "b"), NULL), c("a", "b"))
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

# --- coef_table_mplus: one-tailed p-value message gating ---

test_that("coef_table_mplus does not emit the one-tailed message in est_ci (CrI) mode", {
  skip_if_not_installed("MplusAutomation")

  # Bayesian default is est_ci: significance comes from the credibility interval,
  # so the one-tailed p-value note would be noise and must not be emitted.
  m <- MplusAutomation::readModels(find_extdata("ex5.11_BAYES.out"), quiet = TRUE)
  expect_no_message(coef_table_mplus(m))
  expect_no_message(coef_table_mplus(m, display_type = "est_ci"))
})

test_that("coef_table_mplus emits the one-tailed message in est_se mode for Bayes", {
  skip_if_not_installed("MplusAutomation")

  # est_se displays p-values, so the one-tailed caveat is relevant.
  m <- MplusAutomation::readModels(find_extdata("ex5.11_BAYES.out"), quiet = TRUE)
  expect_message(coef_table_mplus(m, display_type = "est_se"), regexp = "one-tailed")
})

test_that("coef_wrapper pval_note = FALSE suppresses the one-tailed message", {
  skip_if_not_installed("MplusAutomation")

  m <- MplusAutomation::readModels(find_extdata("ex5.11_BAYES.out"), quiet = TRUE)
  expect_message(coef_wrapper(m), regexp = "one-tailed")
  expect_no_message(coef_wrapper(m, pval_note = FALSE))
})

# --- coef_table_mplus: est_ci footnote defines LL/UL ---

test_that("coef_table_mplus est_ci footnote defines LL and UL", {
  skip_if_not_installed("MplusAutomation")
  skip_if_not_installed("gt")

  # Bayesian model -> credibility interval wording.
  mb   <- MplusAutomation::readModels(find_extdata("ex5.11_BAYES.out"), quiet = TRUE)
  html <- as.character(gt::as_raw_html(coef_table_mplus(mb)))
  expect_true(grepl("LL and UL are the lower and upper limits", html))
  expect_true(grepl("credibility interval", html))

  # Frequentist model in est_ci -> confidence interval wording.
  mf   <- MplusAutomation::readModels(find_extdata("ex5.11.out"), quiet = TRUE)
  htmlf <- as.character(gt::as_raw_html(coef_table_mplus(mf, display_type = "est_ci")))
  expect_true(grepl("LL and UL are the lower and upper limits", htmlf))
  expect_true(grepl("confidence interval", htmlf))
})

# --- helpers: random-slope detection and two-level detection ---

test_that("mplus_random_slopes parses the | declaration for ex9.2c", {
  skip_if_not_installed("MplusAutomation")

  m  <- MplusAutomation::readModels(find_extdata("ex9.2c.out"), quiet = TRUE)
  rs <- franzpak:::mplus_random_slopes(m)

  expect_equal(rs$slope, "S")
  expect_equal(rs$definition, "Y ON X")
})

test_that("mplus_random_slopes excludes XWITH latent interactions", {
  # XWITH is declared with `|` too, but the interaction term is a predictor,
  # not a random effect, so it must not appear among the random slopes.
  m <- list(input = list(model = c(
    "!CENG x CSM-C (focal moderation: CSM-C moderates the CENG_T3 -> SCS_T4 path)",
    "CE_CSMC | CENG_T3 XWITH CSMC_T1;",
    "SCS_T4 ON CE_CSMC;"
  )))

  rs <- franzpak:::mplus_random_slopes(m)

  expect_equal(nrow(rs), 0)
  expect_false(any(grepl("XWITH", rs$slope)))
})

test_that("mplus_random_slopes excludes XWITH but keeps real slopes alongside it", {
  m <- list(input = list(model = c(
    "s | y ON x;",
    "f1xf2 | f1 XWITH f2;",
    "y ON f1xf2;"
  )))

  rs <- franzpak:::mplus_random_slopes(m)

  expect_equal(rs$slope, "S")
  expect_equal(rs$kind, "slope")
  expect_false("F1XF2" %in% rs$slope)
})

test_that("mplus_random_slopes flags growth factors as kind = 'growth'", {
  skip_if_not_installed("MplusAutomation")

  # ex6.12 is a single-level growth model (TYPE = RANDOM, no TWOLEVEL): growth
  # factors i/s declared with one `|` and a random slope st for a TVC.
  m  <- MplusAutomation::readModels(find_extdata("ex6.12.out"), quiet = TRUE)
  rs <- franzpak:::mplus_random_slopes(m)

  # Both growth factors from `i s | y1-y4 AT a11-a14` are returned (multi-factor
  # LHS), tagged growth with no outcome/predictor decomposition.
  growth <- rs[rs$kind == "growth", ]
  expect_setequal(growth$slope, c("I", "S"))
  expect_true(all(is.na(growth$outcome)))

  # The random slope st (ON-form) is tagged slope.
  st <- rs[rs$slope == "ST", ]
  expect_true(all(st$kind == "slope"))
  expect_equal(unique(st$outcome), c("Y1", "Y2", "Y3", "Y4"))
})

# --- coef_wrapper: XWITH (latent interaction) ---

test_that("coef_wrapper keeps an XWITH interaction term as an ordinary predictor", {
  skip_if_not_installed("MplusAutomation")

  # ex5.13 is a single-level SEM with a latent interaction f1xf2 | f1 XWITH f2,
  # used as a predictor: f3 ON f1xf2.
  m <- MplusAutomation::readModels(find_extdata("ex5.13.out"), quiet = TRUE)
  r <- coef_wrapper(m, params = "regression")

  # Single-level: no multilevel annotations, plain coefficients only.
  expect_false(any(c("level", "random_slope", "display_level") %in% names(r)))

  # The interaction appears as a predictor of F3, untouched (not re-expressed).
  fx <- r[trimws(r$IV) == "F1XF2", ]
  expect_equal(nrow(fx), 1)
  expect_equal(trimws(fx$DV), "F3")
  expect_equal(fx$est, 0.397, tolerance = 1e-3)

  # F1XF2 is never treated as an outcome / random slope.
  expect_false("F1XF2" %in% trimws(r$DV))
})

test_that("coef_wrapper can relabel an XWITH interaction term", {
  skip_if_not_installed("MplusAutomation")

  m <- MplusAutomation::readModels(find_extdata("ex5.13.out"), quiet = TRUE)
  r <- coef_wrapper(m, params = "regression",
                    label_replace = c("F1XF2" = "F1 x F2"))

  expect_true("F1 x F2" %in% trimws(r$IV))
  expect_false("F1XF2" %in% trimws(r$IV))
})

# --- coef_wrapper: single-level growth model (random effects) ---

test_that("coef_wrapper returns plain coefficients for a single-level growth model", {
  skip_if_not_installed("MplusAutomation")

  # Growth factors / random slopes keep their own names; the regression DVs are
  # not corrupted by the two-level random-slope re-expression.
  m <- MplusAutomation::readModels(find_extdata("ex6.12.out"), quiet = TRUE)
  r <- coef_wrapper(m, params = "regression")

  expect_false(any(c("level", "random_slope", "display_level") %in% names(r)))
  expect_setequal(trimws(r$DV), c("I", "S", "ST"))
  expect_equal(trimws(r$IV), c("X", "X", "X"))
  expect_equal(r$est[trimws(r$DV) == "S"], 0.333, tolerance = 1e-3)
})

test_that("coef_table_mplus auto-detects growth models and shows factor sections", {
  skip_if_not_installed("MplusAutomation")

  m <- MplusAutomation::readModels(find_extdata("ex6.12.out"), quiet = TRUE)
  d <- coef_table_mplus(m, return_data = TRUE)

  expect_equal(d$group, c("Predictors", "Means", "Variances"))
  expect_equal(d$IV, c("X", "Mean", "Residual variance"))
  expect_true(all(c("est_col__I", "est_col__S", "est_col__ST") %in% names(d)))
  expect_false(any(grepl("__Y[1-4]$", names(d))))
  expect_true(grepl("0\\.333\\*", d$est_col__S[d$IV == "X"]))
  expect_true(grepl("1\\.007\\*", d$est_col__S[d$IV == "Mean"]))
  expect_true(grepl("0\\.175\\*", d$est_col__S[d$IV == "Residual variance"]))
})

test_that("model_type can force the default layout for growth-like models", {
  skip_if_not_installed("MplusAutomation")

  m <- MplusAutomation::readModels(find_extdata("ex6.12.out"), quiet = TRUE)
  d <- coef_table_mplus(m, model_type = "default", return_data = TRUE)

  expect_false("group" %in% names(d))
  expect_equal(d$IV, "X")
  expect_true(all(c("est_col__I", "est_col__S", "est_col__ST") %in% names(d)))
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

test_that("coef_table_mplus returns list(fixed, random) for a random-slope model", {
  skip_if_not_installed("MplusAutomation")

  m   <- MplusAutomation::readModels(find_extdata("ex9.2c.out"), quiet = TRUE)
  tbl <- coef_table_mplus(m)

  expect_type(tbl, "list")
  expect_setequal(names(tbl), c("fixed", "random"))

  d <- table_data(tbl$fixed)

  # The main table is split into Within and Between only -- no "Random effects"
  # section, and no separate column for the latent slope S.
  expect_true("level" %in% names(d))
  expect_setequal(unique(d$level), c("Within", "Between"))
  expect_false(any(grepl("__S$", names(d))))

  # The avg. within effect of X on Y appears as predictor X (not "Intercepts")
  # under outcome Y, in the Within section. Formatted gt output footnotes this
  # cell; the raw data stays clean for manipulation.
  within_x <- d[d$level == "Within" & d$IV == "X", ]
  expect_equal(nrow(within_x), 1)
  expect_true(grepl("1\\.017\\*", within_x$est_col__Y))
  expect_false(grepl("\u1d43", within_x$est_col__Y, fixed = TRUE))
  expect_false("Intercepts" %in% d$IV[d$level == "Within"])

  # Within rows come first.
  expect_equal(d$level[1], "Within")

  # The between-person effect of X on Y is a distinct (fixed) coefficient in the
  # Between section, with no random-slope marker.
  between_x <- d[d$level == "Between" & d$IV == "X", ]
  expect_true(grepl("1\\.024\\*", between_x$est_col__Y))
  expect_false(grepl("\u1d43", between_x$est_col__Y, fixed = TRUE))

  # The cross-level interaction (slope S on between-predictor W) stays in the
  # main table, with level suffixes on both components.
  cli <- d[d$level == "Between" & d$IV == "W (between) x X (within)", ]
  expect_equal(nrow(cli), 1)
  expect_true(grepl("0\\.569\\*", cli$est_col__Y))

  # The slope's residual variance is NOT in the main table.
  expect_false("Residual variance" %in% d$IV)
})

test_that("coef_table_mplus random table lists each slope's predictors, mean, variance", {
  skip_if_not_installed("MplusAutomation")

  m   <- MplusAutomation::readModels(find_extdata("ex9.2c.out"), quiet = TRUE)
  re  <- table_data(coef_table_mplus(m)$random)

  # Rows ordered predictors -> mean/intercept -> variance, with cross-level
  # predictors keeping their raw label and mean/variance relabelled by slope.
  expect_true("Parameter" %in% names(re))
  expect_equal(re$Parameter,
               c("S<-W", "S<-X", "S Intercept", "S Residual Variance"))

  # Values come through (cross-level W, mean, residual variance).
  expect_true(grepl("0\\.569\\*", re$est_col[re$Parameter == "S<-W"]))
  expect_true(grepl("1\\.017\\*", re$est_col[re$Parameter == "S Intercept"]))
  expect_true(grepl("0\\.368\\*", re$est_col[re$Parameter == "S Residual Variance"]))
})

test_that("coef_table_mplus two-level gt output has only Within/Between row groups", {
  skip_if_not_installed("MplusAutomation")
  skip_if_not_installed("gt")

  m   <- MplusAutomation::readModels(find_extdata("ex9.2c.out"), quiet = TRUE)
  tbl <- coef_table_mplus(m)

  expect_s3_class(tbl$fixed, "gt_tbl")
  expect_s3_class(tbl$random, "gt_tbl")
  expect_setequal(unique(tbl$fixed[["_row_groups"]]), c("Within", "Between"))

  # The fixed table footnote distinguishes random-slope means and points at the
  # companion Random effects table.
  html <- as.character(gt::as_raw_html(tbl$fixed))
  expect_true(grepl("Random slope intercept", html))
  expect_true(grepl("fixed effects", html))
  expect_true(grepl("Cross-level interactions are shown", html))
  expect_true(grepl("&lt;between predictor&gt; \\(between\\) x", html))
  expect_true(grepl("Random effects table", html))
})

test_that("coef_table_mplus single-level output has no level column", {
  skip_if_not_installed("MplusAutomation")

  m <- MplusAutomation::readModels(find_extdata("ex5.11.out"), quiet = TRUE)
  d <- table_data(coef_table_mplus(m))

  expect_false("level" %in% names(d))
})

test_that("coef_table_mplus defaults to display dashes and can return raw NAs", {
  skip_if_not_installed("MplusAutomation")

  m <- MplusAutomation::readModels(find_extdata("ex5.11.out"), quiet = TRUE)

  d_default <- coef_table_mplus(m, return_data = TRUE)
  value_cols <- setdiff(names(d_default), "IV")
  expect_true(any(unlist(d_default[value_cols]) == "-", na.rm = TRUE))

  d_raw <- coef_table_mplus(m, return_data = TRUE, na_replace = NULL)
  expect_true(any(is.na(unlist(d_raw[value_cols]))))
})

test_that("return_data returns tibbles instead of gt tables", {
  skip_if_not_installed("MplusAutomation")
  skip_if_not_installed("gt")

  m <- MplusAutomation::readModels(find_extdata("ex5.11.out"), quiet = TRUE)

  expect_s3_class(coef_table_mplus(m), "gt_tbl")
  expect_s3_class(coef_table_mplus(m, return_data = TRUE), "tbl_df")
})

# --- interval extraction (long names / CINTERVAL truncation) ---

test_that("mplus_confint_safe reads intervals from the untruncated estimate table", {
  # Mplus truncates variable names to 8 characters in the CINTERVAL section, so
  # the dedicated ci.* table can carry a truncated header (M_APREG_) while the
  # main estimate table keeps the full name (M_APREG_S). Reading intervals from
  # the estimate table keeps the labels consistent with coef().
  fake <- list(parameters = list(
    unstandardized = data.frame(
      paramHeader   = "M_APREG_S.ON",
      param         = "M_EST_A",
      est           = 0.20,
      posterior_sd  = 0.10,
      pval          = 0.01,
      lower_2.5ci   = 0.031,
      upper_2.5ci   = 0.423,
      BetweenWithin = "Within",
      stringsAsFactors = FALSE
    ),
    ci.unstandardized = data.frame(   # truncated header -- must not be used
      paramHeader   = "M_APREG_.ON",
      param         = "M_EST_A",
      low2.5        = 0.031,
      up2.5         = 0.423,
      BetweenWithin = "Within",
      stringsAsFactors = FALSE
    )
  ))

  ci <- franzpak:::mplus_confint_safe(fake, params = "regression", type = "un")

  expect_equal(ci$Label, "W M_APREG_S<-M_EST_A")
  expect_equal(ci$LowerCI, 0.031, tolerance = 1e-12)
  expect_equal(ci$UpperCI, 0.423, tolerance = 1e-12)
})

# --- predictor_order ---

test_that("predictor_order reorders predictor rows; unlisted ones follow", {
  skip_if_not_installed("MplusAutomation")
  skip_if_not_installed("gt")

  m <- MplusAutomation::readModels(find_extdata("ex5.11.out"), quiet = TRUE)

  # Default appearance order.
  expect_equal(table_data(coef_table_mplus(m))$IV, c("F3", "F1", "F2"))

  # Listed predictors come first in the given order; F3 (unlisted) trails.
  d <- table_data(coef_table_mplus(m, predictor_order = c("F2", "F1")))
  expect_equal(d$IV, c("F2", "F1", "F3"))
})

test_that("predictor_order matches displayed labels (after label_replace)", {
  skip_if_not_installed("MplusAutomation")
  skip_if_not_installed("gt")

  m <- MplusAutomation::readModels(find_extdata("ex5.11.out"), quiet = TRUE)
  d <- table_data(coef_table_mplus(
    m,
    label_replace   = c("F1" = "Alpha", "F2" = "Beta"),
    predictor_order = c("Beta", "Alpha")
  ))
  expect_equal(d$IV, c("Beta", "Alpha", "F3"))
})

test_that("predictor_order applies within each section of the two-level fixed table", {
  skip_if_not_installed("MplusAutomation")
  skip_if_not_installed("gt")

  m <- MplusAutomation::readModels(find_extdata("ex9.5_BAYES.out"), quiet = TRUE)
  d <- suppressMessages(table_data(coef_table_mplus(
    m,
    predictor_order = c("X1", "X2")
  )$fixed))

  # Only Within / Between sections now; ordering happens within sections.
  expect_equal(unique(d$level), c("Within", "Between"))
  # Default Within order is X2, X1, Y1; the explicit order puts X1 first.
  expect_equal(d$IV[d$level == "Within"], c("X1", "X2", "Y1"))
})

test_that("predictor_order warns on names absent from the table and validates type", {
  skip_if_not_installed("MplusAutomation")

  m <- MplusAutomation::readModels(find_extdata("ex5.11.out"), quiet = TRUE)
  expect_warning(coef_table_mplus(m, predictor_order = c("F2", "NOPE")),
                 "not in the table")
  expect_error(coef_table_mplus(m, predictor_order = 1:3),
               "must be a character vector")
})

test_that("coef_table_mplus gives no outcome column to predictor-only variables", {
  skip_if_not_installed("MplusAutomation")
  skip_if_not_installed("gt")

  # X1 is a within-only predictor (only ever an IV). Inject a between-level mean
  # for it, mimicking the latent between component Mplus estimates for random
  # effects: it must not earn an outcome column off its mean alone.
  m <- MplusAutomation::readModels(find_extdata("ex9.5_BAYES.out"), quiet = TRUE)
  u <- m$parameters$unstandardized
  newrow <- u[1, ]
  newrow$paramHeader <- "Means"
  newrow$param       <- "X1"
  newrow$est         <- 0.5
  newrow$posterior_sd <- 0.1
  newrow$pval        <- 0.01
  newrow$lower_2.5ci <- 0.3
  newrow$upper_2.5ci <- 0.7
  newrow$BetweenWithin <- "Between"
  m$parameters$unstandardized <- rbind(u, newrow)

  d <- suppressMessages(coef_table_mplus(m)$fixed)[["_data"]]

  # The real outcomes keep their columns; the predictor-only X1 does not.
  expect_true(any(grepl("__Y1$", names(d))))
  expect_true(any(grepl("__Y2$", names(d))))
  expect_false(any(grepl("__X1$", names(d))))
})

test_that("coef_table_mplus drops outcomes that carry only a mean and a variance", {
  skip_if_not_installed("MplusAutomation")
  skip_if_not_installed("gt")

  # A within-only predictor (X1) given both a between-level mean and a within-
  # level variance, mimicking a latent variable whose variance is declared in the
  # Within part purely for decomposition. Such a variable is regressed onto by
  # nothing and must not earn an outcome column off its mean/variance alone.
  m <- MplusAutomation::readModels(find_extdata("ex9.5_BAYES.out"), quiet = TRUE)
  u <- m$parameters$unstandardized
  mk <- function(header, level, est) {
    r <- u[1, ]
    r$paramHeader   <- header
    r$param         <- "X1"
    r$est           <- est
    r$posterior_sd  <- 0.1
    r$pval          <- 0.01
    r$lower_2.5ci   <- est - 0.2
    r$upper_2.5ci   <- est + 0.2
    r$BetweenWithin <- level
    r
  }
  m$parameters$unstandardized <- rbind(
    u, mk("Means", "Between", 0.5), mk("Variances", "Within", 1.2)
  )

  d <- suppressMessages(coef_table_mplus(m)$fixed)[["_data"]]

  expect_true(any(grepl("__Y1$", names(d))))
  expect_true(any(grepl("__Y2$", names(d))))
  expect_false(any(grepl("__X1$", names(d))))
})

test_that("relabelling the 'Means' token does not create spurious outcome columns", {
  skip_if_not_installed("MplusAutomation")

  # `label_replace` rewrites the structural token "Means". Expectation rows must
  # be identified from the Mplus Label (not the relabelled IV), otherwise the
  # between-level means of the predictors X and W make them spurious outcomes.
  m <- MplusAutomation::readModels(
    find_extdata("coef_table_mplus_unnecessary_columns/unnecessary_columns.out"),
    quiet = TRUE
  )
  label_replace <- c("X" = "Predictor X", "W" = "Predictor W",
                     "Y" = "Outcome Y", "Means" = "Mean / intercept")

  d <- table_data(coef_table_mplus(m, label_replace = label_replace,
                                   na_replace = NULL, return_data = TRUE))
  outcomes <- unique(sub("^[^_]+_col__", "", grep("_col__", names(d), value = TRUE)))

  expect_equal(outcomes, "Outcome Y")
  expect_false(any(grepl("__Predictor (X|W)$", names(d))))
  # The mean row still displays under its relabelled name.
  expect_true("Mean / intercept" %in% d$IV)
})

test_that("coef_wrapper(addci=TRUE) leaves no NA intervals for Bayesian models", {
  skip_if_not_installed("MplusAutomation")

  params <- c("regression", "expectation", "variability")
  for (f in c("ex5.11_BAYES.out", "ex9.2c_BAYES.out", "ex9.5_BAYES.out")) {
    m  <- MplusAutomation::readModels(find_extdata(f), quiet = TRUE)
    cw <- suppressMessages(coef_wrapper(m, params = params, addci = TRUE))
    expect_false(any(is.na(cw$LowerCI)), info = f)
    expect_false(any(is.na(cw$UpperCI)), info = f)
  }
})

# --- coef_table_mplus: MODEL CONSTRAINT NEW(...) parameters ---
#
# rsa_constraints.out is a single-level Bayesian RSA with two latent outcomes
# (Z1, Z2); its NEW parameters split by outcome (cs_1..a5_1 -> Z1,
# cs_2..a5_2 -> Z2).

test_that("constraints = FALSE (default) drops NEW parameters even when requested", {
  skip_if_not_installed("MplusAutomation")

  m <- MplusAutomation::readModels(find_extdata("rsa_constraints.out"), quiet = TRUE)
  d <- table_data(coef_table_mplus(m, params = c("regression", "new")))

  # Only the b/bb regression rows survive; no constraint rows, no level column.
  expect_setequal(d$IV, c("X", "Y", "XS", "XY", "YS"))
  expect_false("level" %in% names(d))
  expect_false(any(grepl("\\.additional", names(d))))
  expect_false("CS_1" %in% d$IV)
})

test_that("constraints = TRUE puts unmapped NEW params in the Other group, reusing outcome columns", {
  skip_if_not_installed("MplusAutomation")

  m <- MplusAutomation::readModels(find_extdata("rsa_constraints.out"), quiet = TRUE)
  d <- table_data(coef_table_mplus(m, constraints = TRUE,
                                   return_data = TRUE, na_replace = NULL))

  # No outcome mapping -> all NEW params live in the "Other additional
  # parameters" row group, borrowing the first outcome's (Z1) columns. No
  # separate generic column block is created.
  expect_true("level" %in% names(d))
  expect_setequal(d$level[!is.na(d$level)],
                  rep("Other additional parameters", 10))
  expect_false(any(grepl("\\.additional", names(d))))

  reg <- d[is.na(d$level), ]
  con <- d[!is.na(d$level), ]
  expect_setequal(reg$IV, c("X", "Y", "XS", "XY", "YS"))

  # Values and the CrI-excludes-zero star rule carry over to constraint rows.
  expect_true(grepl("0\\.203\\*", con$est_col__Z1[con$IV == "CS_1"]))
  expect_equal(as.numeric(con$ll_col__Z1[con$IV == "CS_1"]), 0.122,
               tolerance = 1e-3)
  expect_false(grepl("\\*", con$est_col__Z1[con$IV == "A5_1"]))  # CrI spans 0
})

test_that("constraints honour label_replace on the parameter name", {
  skip_if_not_installed("MplusAutomation")

  m <- MplusAutomation::readModels(find_extdata("rsa_constraints.out"), quiet = TRUE)
  d <- table_data(coef_table_mplus(
    m, constraints = TRUE, label_replace = c("CS_1" = "a1 (LOC slope)"),
    return_data = TRUE, na_replace = NULL))

  expect_true("a1 (LOC slope)" %in% d$IV)
  expect_false("CS_1" %in% d$IV)
})

test_that("constraints warns on a mapping to an unknown outcome and falls back", {
  skip_if_not_installed("MplusAutomation")

  m <- MplusAutomation::readModels(find_extdata("rsa_constraints.out"), quiet = TRUE)
  expect_warning(
    tbl <- coef_table_mplus(m, constraints = c(cs_1 = "NOPE"),
                            return_data = TRUE, na_replace = NULL),
    "not in the table"
  )
  d <- table_data(tbl)
  # Unknown-outcome param is shown as unattached (Other group), not under NOPE.
  expect_false(any(grepl("__NOPE$", names(d))))
  expect_equal(d$level[d$IV == "CS_1"], "Other additional parameters")
  expect_true(grepl("0\\.203\\*", d$est_col__Z1[d$IV == "CS_1"]))
})

test_that("constraints warns when the model has no NEW parameters", {
  skip_if_not_installed("MplusAutomation")

  m <- MplusAutomation::readModels(find_extdata("ex5.11.out"), quiet = TRUE)
  expect_warning(coef_table_mplus(m, constraints = TRUE),
                 "no MODEL CONSTRAINT")
})

test_that("constraints gt output uses a row group and no extra column spanner", {
  skip_if_not_installed("MplusAutomation")
  skip_if_not_installed("gt")

  m   <- MplusAutomation::readModels(find_extdata("rsa_constraints.out"), quiet = TRUE)
  tbl <- coef_table_mplus(m, constraints = TRUE)

  expect_s3_class(tbl, "gt_tbl")
  expect_true("Other additional parameters" %in% tbl[["_row_groups"]])
  # Only the outcome column spanners (Z1, Z2); no additional-params spanner.
  expect_setequal(unlist(tbl[["_spanners"]]$spanner_label), c("Z1", "Z2"))
  html <- as.character(gt::as_raw_html(tbl))
  expect_true(grepl("Other additional parameters", html))
  expect_true(grepl("CS_1", html))
})

test_that("constraints work in est_se mode (stars from p-values)", {
  skip_if_not_installed("MplusAutomation")

  m <- MplusAutomation::readModels(find_extdata("rsa_constraints.out"), quiet = TRUE)
  d <- suppressMessages(table_data(coef_table_mplus(
    m, constraints = TRUE, display_type = "est_se",
    return_data = TRUE, na_replace = NULL)))

  expect_true(all(c("est_col__Z1", "se_col__Z1") %in% names(d)))
  expect_false(any(grepl("^ll_col|^ul_col", names(d))))
  expect_false(any(grepl("\\.additional", names(d))))
  con <- d[!is.na(d$level), ]
  expect_equal(as.numeric(con$se_col__Z1[con$IV == "CS_1"]), 0.039,
               tolerance = 1e-3)
})

# --- coef_table_mplus: multi-outcome MODEL CONSTRAINT mapping ---

test_that("constraints map each outcome's NEW params under its own columns", {
  skip_if_not_installed("MplusAutomation")

  m  <- MplusAutomation::readModels(find_extdata("rsa_constraints.out"), quiet = TRUE)
  mp <- c(cs_1 = "Z1", cc_1 = "Z1", is_1 = "Z1", ic_1 = "Z1", a5_1 = "Z1",
          cs_2 = "Z2", cc_2 = "Z2", is_2 = "Z2", ic_2 = "Z2", a5_2 = "Z2")
  d  <- table_data(coef_table_mplus(m, constraints = mp,
                                    return_data = TRUE, na_replace = NULL))

  # Two outcome column blocks; all constraints in one Outcome-specific section.
  expect_true(all(c("est_col__Z1", "est_col__Z2") %in% names(d)))
  expect_setequal(d$level[!is.na(d$level)],
                  rep("Outcome-specific additional parameters", 10))

  # _1 params land in the Z1 columns (Z2 empty); _2 params in Z2 (Z1 empty).
  expect_true(grepl("0\\.203\\*", d$est_col__Z1[d$IV == "CS_1"]))
  expect_true(is.na(d$est_col__Z2[d$IV == "CS_1"]))
  expect_true(grepl("-0\\.204\\*", d$est_col__Z2[d$IV == "CS_2"]))
  expect_true(is.na(d$est_col__Z1[d$IV == "CS_2"]))
})

test_that("multi-outcome partial mapping splits Outcome-specific vs Other", {
  skip_if_not_installed("MplusAutomation")

  # Map only the outcome-1 params to Z1; leave the outcome-2 ones unmapped --
  # they fall into the Other group, borrowing the first outcome (Z1).
  m  <- MplusAutomation::readModels(find_extdata("rsa_constraints.out"), quiet = TRUE)
  mp <- c(cs_1 = "Z1", cc_1 = "Z1", is_1 = "Z1", ic_1 = "Z1", a5_1 = "Z1")
  d  <- table_data(coef_table_mplus(m, constraints = mp,
                                    return_data = TRUE, na_replace = NULL))

  expect_false(any(grepl("\\.additional", names(d))))
  expect_equal(d$level[d$IV == "CS_1"], "Outcome-specific additional parameters")
  expect_equal(d$level[d$IV == "CS_2"], "Other additional parameters")
  # Mapped _1 under Z1; unmapped _2 borrows the first outcome's (Z1) columns.
  expect_true(grepl("0\\.203\\*", d$est_col__Z1[d$IV == "CS_1"]))
  expect_true(grepl("-0\\.204\\*", d$est_col__Z1[d$IV == "CS_2"]))
})

test_that("multi-outcome mapped gt has two outcome spanners and one row group", {
  skip_if_not_installed("MplusAutomation")
  skip_if_not_installed("gt")

  m  <- MplusAutomation::readModels(find_extdata("rsa_constraints.out"), quiet = TRUE)
  mp <- c(cs_1 = "Z1", cc_1 = "Z1", is_1 = "Z1", ic_1 = "Z1", a5_1 = "Z1",
          cs_2 = "Z2", cc_2 = "Z2", is_2 = "Z2", ic_2 = "Z2", a5_2 = "Z2")
  tbl <- coef_table_mplus(m, constraints = mp)

  expect_setequal(unlist(tbl[["_spanners"]]$spanner_label), c("Z1", "Z2"))
  expect_true("Outcome-specific additional parameters" %in% tbl[["_row_groups"]])
  expect_false("Other additional parameters" %in% tbl[["_row_groups"]])
})
