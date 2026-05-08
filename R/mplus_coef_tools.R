#' Separate Mplus output labels into X and Y (internal function)
#'
#' @param model mplus model as input.
#'
#' @return dataframe with coefficients and DV (dependent variable) and IV (independent variable) labels.
# #' @export # do not export
#'
#' @noRd
sep_label_mplus <- function(model) {
  # takes the output of coef() as input (a data.frame with a Label column)
  modelcoefs <- model
  newlabels <- map_dfr(array_branch(modelcoefs |> select("Label"), 1), function(x) {
    xout <- bind_cols(as_tibble(t(x)), tibble(DV = NA, IV = NA))
    if(str_detect(xout$Label, "<-")) {
      xout <- xout |> mutate(DV = str_extract(xout$Label, ".*(?=<-)"),
                             IV = str_extract(xout$Label, "(?<=<-).*"))
    }
    return(xout)
  })
  coefout <- bind_cols(modelcoefs, newlabels |> select(DV, IV))
  return(coefout)
}

#' Detect whether a Mplus model used Bayesian estimation
#'
#' @param model Mplus model object from MplusAutomation.
#' @return Logical; TRUE if ESTIMATOR = BAYES was found in the model input.
#' @noRd
detect_mplus_bayes <- function(model) {
  isTRUE(toupper(model$input$analysis$estimator) == "BAYES")
}

#' Format Mplus Model Coefficients
#'
#' @param model the Mplus model from MplusAutomation.
#' @param label_replace named character vector for replacing parameter labels
#' @param params which types of parameters to extract, see MplusAutomation
#' @param bayes `NULL` (default) to auto-detect from the model estimator; `TRUE`
#'   to force Bayesian display (p-values doubled, CrI-based significance);
#'   `FALSE` to force frequentist display. A warning is issued when the explicit
#'   value conflicts with the detected estimator.
#' @param addci Add confidence intervals to parameters
#'
#' @return a dataframe with mplus model coefficients
#' @export
#'
#' @examples
#' \dontrun{
#' # get coefficients including confidence intervals
#' mplusmodels <- MplusAutomation::readModels("inst/extdata")
#' coef_wrapper(mplusmodels$ex5.11.out, params = c('regression', 'new'), addci = TRUE)
#' }
coef_wrapper <- function(model, label_replace = NULL, params = c('regression'), bayes = NULL, addci = FALSE) {
  detected_bayes <- detect_mplus_bayes(model)

  if (is.null(bayes)) {
    bayes <- detected_bayes
  } else if (isTRUE(bayes) && !detected_bayes) {
    warning(
      "bayes = TRUE was specified but the model estimator does not appear to be BAYES. ",
      "Proceeding with Bayesian p-value adjustment (p-values doubled)."
    )
  } else if (isFALSE(bayes) && detected_bayes) {
    warning(
      "The model was estimated with BAYES but bayes = FALSE. ",
      "Set bayes = TRUE (or leave NULL for auto-detection) for CrI-based significance."
    )
  }

  testcoefs <- coef(model, params = params)

  if (bayes) {
    testcoefs <- testcoefs |> mutate(pval = pval * 2)
  }

  if (addci) {
    testcoefs <- left_join(testcoefs, confint(model, params), by = "Label")
  }

  testcoefs <- sep_label_mplus(testcoefs)

  if (!is.null(label_replace)) {
    testcoefs <- testcoefs |>
      mutate(DV = str_replace_all(DV, label_replace),
             IV = str_replace_all(IV, label_replace))
  }

  testcoefs
}

#' Wide regression coefficient table from Mplus model
#'
#' Produces a predictor × outcome table with nested sub-columns for each outcome.
#' For ML models: estimate (starred when p < `sig_threshold`) and SE; optionally 95% CIs.
#' For Bayesian models: estimate (starred when 95% CrI excludes zero), CrI lower, CrI upper.
#' The estimator is auto-detected from the model when `bayes = NULL` (default).
#' Returns a `gt` table when the **gt** package is available, otherwise a plain tibble.
#'
#' @param model Mplus model object from MplusAutomation.
#' @param label_replace Named character vector for replacing DV/IV labels.
#' @param params Which parameter types to extract (passed to MplusAutomation).
#' @param bayes `NULL` (default) to auto-detect; `TRUE`/`FALSE` to override.
#'   Warnings from conflicting explicit values are issued by [coef_wrapper()].
#' @param addci Logical; include 95% CI columns for ML models (ignored when Bayesian).
#' @param sig_threshold P-value threshold for significance stars (ML only, default 0.05).
#' @param digits Number of decimal places (default 3).
#' @param na_replace Character string to substitute for empty cells (predictors that
#'   do not enter a given outcome). Common choices: `"-"` or `""`. `NULL` (default)
#'   leaves NAs as-is.
#'
#' @return A `gt` table, or a plain tibble when **gt** is not installed.
#' @export
#'
#' @examples
#' \dontrun{
#' m <- MplusAutomation::readModels("inst/extdata/ex5.11.out")
#' coef_table_mplus(m)
#' coef_table_mplus(m, addci = TRUE)
#' coef_table_mplus(m, na_replace = "-")
#' coef_table_mplus(m, label_replace = c("F3" = "Mediator", "F4" = "Outcome"))
#' }
coef_table_mplus <- function(model, label_replace = NULL, params = c('regression'),
                              bayes = NULL, addci = FALSE, sig_threshold = 0.05,
                              digits = 3, na_replace = NULL) {

  # Resolve effective_bayes for column-structure decisions here (silent);
  # conflict warnings are handled inside coef_wrapper when it receives the
  # user's original bayes value.
  effective_bayes <- if (is.null(bayes)) detect_mplus_bayes(model) else bayes

  fetch_ci <- effective_bayes || addci

  coefs <- coef_wrapper(model, label_replace = label_replace, params = params,
                        bayes = bayes, addci = fetch_ci) |>
    filter(!is.na(DV)) |>
    mutate(DV = trimws(DV), IV = trimws(IV))

  dvs <- unique(coefs$DV)
  fmt <- function(x) format(round(x, digits), nsmall = digits)

  if (effective_bayes) {
    long <- coefs |>
      mutate(
        sig     = !(LowerCI <= 0 & UpperCI >= 0),
        est_col = paste0(fmt(est), if_else(sig, "*", "")),
        ll_col  = fmt(LowerCI),
        ul_col  = fmt(UpperCI)
      ) |>
      select(IV, DV, est_col, ll_col, ul_col)

    value_cols <- c("est_col", "ll_col", "ul_col")
    sub_labels <- c("Est.", "LL", "UL")
    footnote   <- "* 95% credibility interval excludes zero"

  } else {
    long <- coefs |>
      mutate(
        sig     = !is.na(pval) & pval < sig_threshold,
        est_col = paste0(fmt(est), if_else(sig, "*", "")),
        se_col  = fmt(se)
      )

    if (addci) {
      long <- long |>
        mutate(ll_col = fmt(LowerCI), ul_col = fmt(UpperCI)) |>
        select(IV, DV, est_col, se_col, ll_col, ul_col)
      value_cols <- c("est_col", "se_col", "ll_col", "ul_col")
      sub_labels <- c("Est.", "SE", "LL", "UL")
    } else {
      long <- long |> select(IV, DV, est_col, se_col)
      value_cols <- c("est_col", "se_col")
      sub_labels <- c("Est.", "SE")
    }
    footnote <- paste0("* p < ", sig_threshold)
  }

  # Pivot to wide with column names {value}__{DV}
  wide <- long |>
    pivot_wider(
      names_from  = DV,
      values_from = all_of(value_cols),
      names_glue  = "{.value}__{DV}"
    )

  # Reorder so each DV's sub-columns are grouped together
  ordered_cols <- c("IV", unlist(lapply(dvs, function(dv) {
    paste0(value_cols, "__", dv)
  })))
  wide <- wide |> select(all_of(ordered_cols))

  if (!is.null(na_replace)) {
    wide <- wide |>
      mutate(across(-all_of("IV"), \(x) if_else(is.na(x), na_replace, x)))
  }

  if (!requireNamespace("gt", quietly = TRUE)) {
    return(wide)
  }

  # Build per-column label list for gt
  col_labels_list <- list(IV = "Predictor")
  for (dv in dvs) {
    for (i in seq_along(value_cols)) {
      col_labels_list[[paste0(value_cols[i], "__", dv)]] <- sub_labels[i]
    }
  }

  gt_tbl <- gt::gt(wide) |>
    gt::cols_label(.list = col_labels_list) |>
    gt::tab_footnote(footnote = footnote)

  # One spanner per outcome
  for (dv in dvs) {
    gt_tbl <- gt_tbl |>
      gt::tab_spanner(label = dv, columns = paste0(value_cols, "__", dv))
  }

  gt_tbl
}

#' In beta/questioning status. Useful only for indirect effects, but cannot coef_wrapper() be used directly?
#'
#' @param model
#'
#' @return a tibble containing parameter and confidence intervals
# #' @export # do not export
#' @noRd
confint_wrapper <- function(model) {
  message("Experimental function, may be deprecated in favor coef_wrapper()")
  coefs <- coef(model, params = 'new') |> select(Label, est)
  confints <- confint(model, params = c('new'))
  left_join(coefs, confints, by = "Label")
}
