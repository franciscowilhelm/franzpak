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

#' Detect whether a Mplus model is a two-level (multilevel) model
#'
#' Two-level models carry a `BetweenWithin` column in the parsed parameter
#' table, distinguishing Within- and Between-level parameters.
#'
#' @param model Mplus model object from MplusAutomation.
#' @return Logical; TRUE if the model has Within/Between parameter sections.
#' @noRd
is_mplus_twolevel <- function(model) {
  isTRUE("BetweenWithin" %in% names(model$parameters$unstandardized))
}

#' Identify random slopes declared in the MODEL command
#'
#' Random slopes are latent variables defined with the `|` operator, e.g.
#' `s | y ON x`. The intercept/mean of such a slope is the between-person mean
#' of a within-person effect, which [coef_table_mplus()] reports in the Within
#' section of the table.
#'
#' @param model Mplus model object from MplusAutomation.
#' @return A tibble with one row per random slope and columns `slope`
#'   (upper-cased slope name), `definition` (the within-level relationship it
#'   captures, e.g. `"Y ON X"`), `outcome` (the DV of that relationship, e.g.
#'   `"Y"`) and `predictor` (the IV, e.g. `"X"`). Empty tibble when no random
#'   slopes are found.
#' @noRd
mplus_random_slopes <- function(model) {
  empty <- tibble(slope = character(0), definition = character(0),
                  outcome = character(0), predictor = character(0))
  modlines <- model$input$model
  if (is.null(modlines) || length(modlines) == 0) return(empty)

  statements <- unlist(strsplit(paste(modlines, collapse = "\n"), ";"))
  bar_stmts  <- statements[str_detect(statements, "\\|")]
  if (length(bar_stmts) == 0) return(empty)

  map_dfr(bar_stmts, function(stmt) {
    parts <- strsplit(stmt, "\\|")[[1]]
    # Strip %WITHIN% / %BETWEEN% section headers that may share the chunk.
    lhs <- gsub("%[^%]*%", "", parts[1])
    rhs <- if (length(parts) >= 2) parts[2] else ""
    slopes <- toupper(unlist(strsplit(trimws(lhs), "\\s+")))
    slopes <- slopes[nzchar(slopes)]
    if (length(slopes) == 0) {
      return(tibble(slope = character(0), definition = character(0),
                    outcome = character(0), predictor = character(0)))
    }
    definition <- toupper(trimws(gsub("\\s+", " ", rhs)))
    # Split the "Y ON X" relationship into outcome (Y) and predictor (X).
    outcome   <- trimws(sub("\\bON\\b.*$", "", definition))
    predictor <- trimws(sub("^.*\\bON\\b", "", definition))
    tibble(slope = slopes, definition = definition,
           outcome = outcome, predictor = predictor)
  })
}

#' Extract Mplus confidence/credibility intervals robustly (internal)
#'
#' [stats::confint()] for `mplus.model` objects only reads the dedicated
#' `ci.*` parameter tables, which Mplus produces with the CINTERVAL option. It
#' errors on two-level models that lack those tables, even when the estimate
#' table already carries interval columns (as Bayesian output does). This helper
#' tries `confint()` first and falls back to reconstructing intervals from the
#' estimate table, producing labels that match those from [stats::coef()].
#'
#' @param model Mplus model object from MplusAutomation.
#' @param params Parameter types to extract.
#' @param type Coefficient scale (`"un"`, `"stdyx"`, ...).
#' @return A tibble with `Label`, `LowerCI`, `UpperCI`, or `NULL` if no
#'   intervals are available.
#' @noRd
mplus_confint_safe <- function(model, params, type) {
  ci <- tryCatch(
    suppressWarnings(confint(model, params = params, type = type)),
    error = function(e) NULL
  )
  if (is.data.frame(ci) && nrow(ci) > 0 &&
      all(c("Label", "LowerCI", "UpperCI") %in% names(ci))) {
    return(tibble(Label = ci$Label, LowerCI = ci$LowerCI, UpperCI = ci$UpperCI))
  }
  mplus_confint_fallback(model, params, type)
}

#' Reconstruct intervals from the Mplus estimate table (internal)
#'
#' @inheritParams mplus_confint_safe
#' @return A tibble with `Label`, `LowerCI`, `UpperCI`, or `NULL`.
#' @noRd
mplus_confint_fallback <- function(model, params, type) {
  tbl_name <- switch(type,
    un    = "unstandardized",
    std   = "std.standardized",
    stdy  = "stdy.standardized",
    stdyx = "stdyx.standardized",
    NULL
  )
  d <- model$parameters[[tbl_name]]
  if (is.null(d)) return(NULL)

  lo <- intersect(c("lower_2.5ci", "low2.5"), names(d))
  hi <- intersect(c("upper_2.5ci", "up2.5"), names(d))
  if (length(lo) == 0 || length(hi) == 0) return(NULL)
  lo <- lo[1]; hi <- hi[1]

  ph <- d$paramHeader
  ptype <- dplyr::case_when(
    str_detect(ph, "\\.ON$")            ~ "regression",
    str_detect(ph, "\\.BY$|\\.\\|$")    ~ "loading",
    str_detect(ph, "\\.WITH$")          ~ "undirected",
    ph %in% c("Means", "Intercepts", "Thresholds") ~ "expectation",
    str_detect(ph, "Variances")         ~ "variability",
    str_detect(ph, "New.Additional")    ~ "new",
    TRUE                                ~ NA_character_
  )
  keep <- ptype %in% params
  if (!any(keep)) return(NULL)
  d <- d[keep, , drop = FALSE]
  ptype <- ptype[keep]

  label <- vapply(seq_len(nrow(d)), function(i) {
    h <- d$paramHeader[i]; p <- d$param[i]
    switch(ptype[i],
      regression  = paste0(gsub("\\.ON", "<-", h), p),
      undirected  = paste0(gsub("\\.WITH", "<->", h), p),
      expectation = paste0(p, "<-", gsub("\\.Means|\\.Intercepts|\\.Thresholds", "", h)),
      variability = paste0(p, "<->", p),
      loading     = paste0(p, "<-", gsub("\\.BY|\\.\\|", "", h)),
      new         = p,
      NA_character_
    )
  }, character(1))

  prefix <- if ("BetweenWithin" %in% names(d)) paste0(substr(d$BetweenWithin, 1, 1), " ") else ""

  tibble(Label = paste0(prefix, label), LowerCI = d[[lo]], UpperCI = d[[hi]])
}

#' Format Mplus Model Coefficients
#'
#' Extracts regression (and optionally other) coefficients from an
#' MplusAutomation model and splits the Mplus parameter labels into dependent
#' (`DV`) and independent (`IV`) variable columns.
#'
#' For two-level models the output gains multilevel annotations (these columns
#' are omitted entirely for single-level models, where they would carry no
#' information):
#' * `level` -- the Mplus section the parameter belongs to (`"Within"` or
#'   `"Between"`), parsed from the coefficient label prefix.
#' * `random_slope` -- `TRUE` when the parameter pertains to a random slope
#'   declared with `|` in the MODEL command (e.g. `s | y ON x`): its mean,
#'   its cross-level predictors, or its variance.
#' * `within_avg_effect` -- `TRUE` for the intercept/mean of a random slope.
#'   These parameters are the between-person means of within-person effects.
#'   For these rows the opaque `DV`/`IV` (e.g. slope `S` "ON" `Intercepts`) is
#'   re-expressed as the within-person relationship the slope captures, so that
#'   `s | y ON x` is reported with `DV = "Y"` and `IV = "X"`.
#' * `random_effect` -- `TRUE` for the other parameters of a random slope: its
#'   cross-level predictors (`s ON w`, reported with `DV` set to the slope's
#'   outcome) and its (residual) variance (reported with `IV` set to
#'   `"Residual variance"`/`"Variance"`). When several random slopes share an
#'   outcome column, these `IV` labels are qualified with the slope they belong
#'   to, e.g. `"W (Y on X)"`, so they stay unambiguous.
#' * `variance` -- `TRUE` for (residual) variance parameters.
#' * `display_level` -- the section [coef_table_mplus()] places the parameter
#'   in: `"Within"` (within-person effects, including random-slope means),
#'   `"Between"` (between-person effects), or `"Random effects"` (a random
#'   slope's cross-level predictors and variance).
#'
#' @param model the Mplus model from MplusAutomation.
#' @param label_replace named character vector for replacing parameter labels
#' @param params which types of parameters to extract, see MplusAutomation
#' @param type coefficient scale passed to [stats::coef()] and [stats::confint()]
#'   methods for `mplus.model` objects. Common values: `"un"` (default,
#'   unstandardized), `"stdyx"`, `"stdy"`, `"std"`.
#' @param bayes `NULL` (default) to auto-detect from the model estimator; `TRUE`
#'   to signal a Bayesian model (enables CrI-based significance in
#'   [coef_table_mplus()]); `FALSE` to force frequentist display. A warning is
#'   issued when the explicit value conflicts with the detected estimator. When
#'   a Bayesian model is detected, a message notes that Mplus p-values are
#'   one-tailed.
#' @param addci Add confidence intervals to parameters
#'
#' @return a dataframe with mplus model coefficients
#' @export
#'
#' @examples
#' \dontrun{
#' # unstandardized coefficients with CIs
#' mplusmodels <- MplusAutomation::readModels("inst/extdata")
#' coef_wrapper(mplusmodels$ex5.11.out, params = c('regression', 'new'), addci = TRUE)
#' # fully standardized
#' coef_wrapper(mplusmodels$ex5.11.out, type = "stdyx")
#' # two-level model with a random slope: note the level / random_slope columns
#' coef_wrapper(mplusmodels$ex9.2c.out, params = c('regression', 'expectation'))
#' }
coef_wrapper <- function(model, label_replace = NULL, params = c('regression'), type = "un", bayes = NULL, addci = FALSE) {
  detected_bayes <- detect_mplus_bayes(model)

  if (detected_bayes) {
    message("Bayesian estimator detected: p-values in this output are one-tailed.")
  }

  if (is.null(bayes)) {
    bayes <- detected_bayes
  } else if (isTRUE(bayes) && !detected_bayes) {
    warning(
      "bayes = TRUE was specified but the model estimator does not appear to be BAYES."
    )
  } else if (isFALSE(bayes) && detected_bayes) {
    warning(
      "The model was estimated with BAYES but bayes = FALSE. ",
      "Set bayes = TRUE (or leave NULL for auto-detection) for CrI-based significance."
    )
  }

  testcoefs <- coef(model, params = params, type = type)

  if (addci) {
    cis <- mplus_confint_safe(model, params = params, type = type)
    if (is.null(cis)) {
      warning(
        "Confidence/credibility intervals are not available for this model ",
        "(no CINTERVAL output and no interval columns); returning estimates only."
      )
    } else {
      testcoefs <- left_join(testcoefs, cis, by = "Label")
    }
  }

  # Parse and strip the multilevel section prefix ("B " / "W ") that the
  # coef() method prepends to two-level labels.
  testcoefs <- testcoefs |>
    mutate(
      level = dplyr::case_when(
        str_detect(Label, "^W ") ~ "Within",
        str_detect(Label, "^B ") ~ "Between",
        TRUE                     ~ NA_character_
      ),
      Label = stringr::str_remove(Label, "^[BW] ")
    )

  testcoefs <- sep_label_mplus(testcoefs)

  # (Co)variance parameters use the "<->" operator, which sep_label_mplus does
  # not split cleanly; recover both sides directly from the label.
  und <- str_detect(testcoefs$Label, "<->")
  if (any(und)) {
    sides <- stringr::str_split_fixed(testcoefs$Label[und], "<->", 2)
    testcoefs$DV[und] <- sides[, 1]
    testcoefs$IV[und] <- sides[, 2]
  }

  # Classify parameters that involve a random slope (a latent slope declared
  # with `|`, e.g. `s | y ON x`).
  slopes_tbl  <- mplus_random_slopes(model)
  slope_names <- slopes_tbl$slope

  dv_u <- toupper(trimws(testcoefs$DV))
  iv_u <- toupper(trimws(testcoefs$IV))

  is_slope_dv <- !is.na(testcoefs$DV) & dv_u %in% slope_names
  is_variance <- und & (dv_u == iv_u)
  is_expect   <- trimws(testcoefs$IV) %in% c("Intercepts", "Means", "Thresholds")

  within_avg_effect <- is_slope_dv & is_expect                 # mean/intercept of slope
  crosslevel_reg    <- is_slope_dv & !und & !is_expect         # slope ON between-predictor
  slope_variance    <- is_slope_dv & is_variance               # (residual) variance of slope

  # A slope that is itself regressed has a *residual* variance.
  regressed_slopes <- unique(dv_u[crosslevel_reg])

  testcoefs <- testcoefs |>
    mutate(
      random_slope      = is_slope_dv,
      within_avg_effect = within_avg_effect,
      random_effect     = crosslevel_reg | slope_variance,
      variance          = is_variance,
      display_level     = dplyr::case_when(
        within_avg_effect            ~ "Within",
        crosslevel_reg | slope_variance ~ "Random effects",
        TRUE                         ~ level
      )
    )

  # Re-express slope parameters in terms of the within-person relationship the
  # slope captures (`s | y ON x`), so the table shows the real predictor/outcome
  # instead of the latent slope name. Done before label_replace so user
  # replacements apply to the resolved predictor/outcome names.
  idx <- match(dv_u, slope_names)

  # A concise tag identifying each slope ("Y on X"), used to disambiguate the
  # Random-effects rows. It is only needed when two or more random slopes share
  # the same outcome column -- otherwise the rows already live in different
  # columns and plain labels are unambiguous. Built from the slope's
  # outcome/predictor names so any label_replace propagates into the tag.
  slope_tag  <- paste(slopes_tbl$outcome, "on", slopes_tbl$predictor)
  shared_out <- as.integer(table(slopes_tbl$outcome)[slopes_tbl$outcome]) > 1

  # mean/intercept -> the average within effect of x on y (predictor x, outcome y)
  rmean <- which(within_avg_effect & !is.na(idx))
  testcoefs$IV[rmean] <- slopes_tbl$predictor[idx[rmean]]
  testcoefs$DV[rmean] <- slopes_tbl$outcome[idx[rmean]]

  # cross-level interaction -> keep the between predictor, move under outcome y
  rcl <- which(crosslevel_reg & !is.na(idx))
  testcoefs$DV[rcl] <- slopes_tbl$outcome[idx[rcl]]
  q <- shared_out[idx[rcl]]
  testcoefs$IV[rcl[q]] <- paste0(testcoefs$IV[rcl[q]], " (", slope_tag[idx[rcl[q]]], ")")

  # (residual) variance -> a labelled row under outcome y
  rvar  <- which(slope_variance & !is.na(idx))
  vtype <- ifelse(dv_u[rvar] %in% regressed_slopes, "Residual variance", "Variance")
  q     <- shared_out[idx[rvar]]
  vtype[q] <- paste0(vtype[q], " (", slope_tag[idx[rvar[q]]], ")")
  testcoefs$IV[rvar] <- vtype
  testcoefs$DV[rvar] <- slopes_tbl$outcome[idx[rvar]]

  if (!is.null(label_replace)) {
    testcoefs <- testcoefs |>
      mutate(DV = str_replace_all(DV, label_replace),
             IV = str_replace_all(IV, label_replace))
  }

  # The multilevel annotations only carry information for two-level models; for
  # single-level models they are constant (NA / FALSE), so drop them.
  if (!is_mplus_twolevel(model)) {
    testcoefs <- testcoefs |>
      select(-all_of(c("level", "random_slope", "within_avg_effect",
                       "random_effect", "variance", "display_level")))
  }

  testcoefs
}

#' Wide regression coefficient table from Mplus model
#'
#' Produces a predictor x outcome table with nested sub-columns for each outcome.
#' Display is controlled by `display_type`:
#' - `"est_se"`: estimate (starred when p < `sig_threshold`) and SE. When the
#'   model is Bayesian the footnote notes that p-values are one-tailed.
#' - `"est_ci"`: estimate (starred when 95% CI/CrI excludes zero), LL, UL.
#'
#' `display_type` is auto-detected from the estimator when `NULL` (default):
#' Bayesian models -> `"est_ci"`, all others -> `"est_se"`.
#' The estimator is auto-detected from the model when `bayes = NULL` (default).
#' Returns a `gt` table when the **gt** package is available, otherwise a plain tibble.
#'
#' ## Two-level models
#'
#' For two-level models the table is split into up to three sections (row groups
#' in `gt`, a leading `level` column in the plain tibble):
#' * **Within** -- within-person effects, including the means/intercepts of
#'   random slopes (latent slopes declared with `|`, e.g. `s | y ON x`). A slope
#'   mean is the between-person mean of a within-person effect and is shown under
#'   the predictor it represents (`x`), marked with a superscript letter.
#' * **Between** -- between-person effects on the real outcomes.
#' * **Random effects** -- the remaining parameters of each random slope: its
#'   between-level predictors (cross-level interactions, `s ON w`) and its
#'   (residual) variance. These are shown under the slope's outcome column rather
#'   than giving the latent slope its own outcome column.
#'
#' For two-level models `params` defaults to
#' `c("regression", "expectation", "variability")` so means/intercepts and the
#' random-slope variance are included; variances that do not belong to a random
#' slope are omitted from the table.
#'
#' @param model Mplus model object from MplusAutomation.
#' @param label_replace Named character vector for replacing DV/IV labels.
#' @param params Which parameter types to extract (passed to MplusAutomation).
#'   `NULL` (default) resolves to `c("regression", "expectation", "variability")`
#'   for two-level models and `"regression"` otherwise.
#' @param bayes `NULL` (default) to auto-detect; `TRUE`/`FALSE` to override.
#'   Warnings from conflicting explicit values are issued by [coef_wrapper()].
#' @param display_type `NULL` (default, auto-detect), `"est_se"` (estimate + SE,
#'   p-value stars), or `"est_ci"` (estimate + CI LL/UL, interval-exclusion stars).
#' @param type coefficient scale passed through to [coef_wrapper()]. Default `"un"`.
#' @param sig_threshold P-value threshold for significance stars (`"est_se"` only, default 0.05).
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
#' coef_table_mplus(m, display_type = "est_ci")
#' coef_table_mplus(m, na_replace = "-")
#' coef_table_mplus(m, label_replace = c("F3" = "Mediator", "F4" = "Outcome"))
#' # two-level model: Within/Between sections, random-slope mean in Within
#' m2 <- MplusAutomation::readModels("inst/extdata/ex9.2c.out")
#' coef_table_mplus(m2)
#' }
coef_table_mplus <- function(model, label_replace = NULL, params = NULL,
                              bayes = NULL, display_type = NULL, type = "un",
                              sig_threshold = 0.05, digits = 3, na_replace = NULL) {

  twolevel <- is_mplus_twolevel(model)

  if (is.null(params)) {
    params <- if (twolevel) c("regression", "expectation", "variability") else c("regression")
  }

  # Resolve effective_bayes (silent); conflict warnings handled in coef_wrapper.
  effective_bayes <- if (is.null(bayes)) detect_mplus_bayes(model) else bayes

  effective_display <- if (is.null(display_type)) {
    if (effective_bayes) "est_ci" else "est_se"
  } else {
    display_type
  }

  fetch_ci <- effective_display == "est_ci"

  coefs <- coef_wrapper(model, label_replace = label_replace, params = params,
                        type = type, bayes = bayes, addci = fetch_ci) |>
    filter(!is.na(DV)) |>
    mutate(DV = trimws(DV), IV = trimws(IV))

  # Drop variances that do not belong to a random slope (e.g. outcome residual
  # variances): only the random-slope (residual) variance is tabulated.
  if (twolevel) {
    coefs <- coefs |> filter(!(variance & !random_effect))
  }

  # If intervals were requested but could not be retrieved, fall back to est/SE.
  if (fetch_ci && !all(c("LowerCI", "UpperCI") %in% names(coefs))) {
    warning("Intervals unavailable for this model; showing estimates and standard errors instead.")
    effective_display <- "est_se"
    fetch_ci <- FALSE
  }

  # Section column and random-slope-mean flag: present/meaningful only for
  # two-level models. `display_level`/`within_avg_effect` exist on `coefs` only
  # when two-level, so guard the references.
  coefs <- coefs |>
    mutate(
      group   = if (twolevel) display_level     else NA_character_,
      rs_mean = if (twolevel) within_avg_effect else FALSE
    )

  # APA specific-note marker: a superscript lowercase letter (the dagger is
  # reserved for marginal significance in APA, so it is avoided here).
  rs_marker <- "\u1d43"  # superscript "a"

  dvs <- unique(coefs$DV)
  fmt <- function(x) format(round(x, digits), nsmall = digits)

  if (effective_display == "est_ci") {
    long <- coefs |>
      mutate(
        sig     = !(LowerCI <= 0 & UpperCI >= 0),
        est_col = paste0(fmt(est), if_else(sig, "*", ""),
                         if_else(rs_mean, rs_marker, "")),
        ll_col  = fmt(LowerCI),
        ul_col  = fmt(UpperCI)
      ) |>
      select(group, IV, DV, est_col, ll_col, ul_col)

    value_cols <- c("est_col", "ll_col", "ul_col")
    sub_labels <- c("Est.", "LL", "UL")
    footnote   <- if (effective_bayes) {
      "* 95% credibility interval excludes zero."
    } else {
      "* 95% confidence interval excludes zero."
    }

  } else {
    long <- coefs |>
      mutate(
        sig     = !is.na(pval) & pval < sig_threshold,
        est_col = paste0(fmt(est), if_else(sig, "*", ""),
                         if_else(rs_mean, rs_marker, "")),
        se_col  = fmt(se)
      ) |>
      select(group, IV, DV, est_col, se_col)

    value_cols <- c("est_col", "se_col")
    sub_labels <- c("Est.", "SE")
    footnote   <- if (effective_bayes) {
      paste0("* p < ", sig_threshold, ", one-tailed.")
    } else {
      paste0("* p < ", sig_threshold)
    }
  }

  # Pivot to wide with column names {value}__{DV}; rows keyed by (group, IV).
  wide <- long |>
    pivot_wider(
      names_from  = DV,
      values_from = all_of(value_cols),
      names_glue  = "{.value}__{DV}"
    )

  # Reorder so each DV's sub-columns are grouped together.
  ordered_cols <- c("group", "IV", unlist(lapply(dvs, function(dv) {
    paste0(value_cols, "__", dv)
  })))
  wide <- wide |> select(all_of(ordered_cols))

  if (!is.null(na_replace)) {
    wide <- wide |>
      mutate(across(-all_of(c("group", "IV")), \(x) if_else(is.na(x), na_replace, x)))
  }

  section_order <- c("Within", "Between", "Random effects")

  if (twolevel) {
    # Order Within, Between, Random effects; preserve predictor order within each.
    section_levels <- intersect(section_order, unique(wide$group))
    wide <- wide |>
      mutate(group = factor(group, levels = section_levels)) |>
      dplyr::arrange(group) |>
      mutate(group = as.character(group)) |>
      dplyr::rename(level = group)
  } else {
    wide <- wide |> select(-all_of("group"))
  }

  if (!requireNamespace("gt", quietly = TRUE)) {
    return(wide)
  }

  # Build per-column label list for gt.
  col_labels_list <- list(IV = "Predictor")
  for (dv in dvs) {
    for (i in seq_along(value_cols)) {
      col_labels_list[[paste0(value_cols[i], "__", dv)]] <- sub_labels[i]
    }
  }

  if (twolevel) {
    gt_tbl <- gt::gt(wide, groupname_col = "level")
  } else {
    gt_tbl <- gt::gt(wide)
  }

  gt_tbl <- gt_tbl |>
    gt::cols_label(.list = col_labels_list) |>
    gt::tab_footnote(footnote = footnote)

  # One spanner per outcome.
  for (dv in dvs) {
    gt_tbl <- gt_tbl |>
      gt::tab_spanner(label = dv, columns = paste0(value_cols, "__", dv))
  }

  if (twolevel) {
    section_levels <- intersect(section_order, unique(wide$level))
    gt_tbl <- gt_tbl |> gt::row_group_order(groups = section_levels)

    # Note distinguishing random-slope means/intercepts from fixed effects.
    if (isTRUE(any(coefs$rs_mean))) {
      rs_note <- paste0(
        rs_marker,
        " Random-slope mean/intercept: the between-person mean of a within-person ",
        "effect (shown in the Within section under the predictor it represents). ",
        "All other coefficients are fixed effects."
      )
      gt_tbl <- gt_tbl |> gt::tab_footnote(footnote = rs_note)
    }

    # Note explaining the Random effects section.
    if (isTRUE(any(coefs$display_level == "Random effects"))) {
      rs_defs <- mplus_random_slopes(model)
      slope_txt <- if (nrow(rs_defs) > 0) {
        paste0(" Random slope(s): ",
               paste0(tolower(rs_defs$definition), collapse = "; "), ".")
      } else ""
      re_note <- paste0(
        "Random effects: between-level predictors of the random slope ",
        "(cross-level interactions) and its (residual) variance.", slope_txt
      )
      gt_tbl <- gt_tbl |> gt::tab_footnote(footnote = re_note)
    }
  }

  gt_tbl
}

#' In beta/questioning status. Useful only for indirect effects, but cannot coef_wrapper() be used directly?
#'
#' @param model Mplus model object from MplusAutomation.
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
