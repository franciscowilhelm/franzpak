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

#' Identify random effects declared with the `|` operator in the MODEL command
#'
#' Mplus overloads `|` for three different constructs: random effects (random
#' intercepts/slopes in multilevel models), growth factors (growth models), and
#' latent variable interactions (`XWITH`). The first two are random effects --
#' latent variables with a mean, a variance, and possibly regressions on other
#' variables -- and there is no reliable syntactic way to tell a growth model
#' apart from a multilevel random-slope model, so this helper treats them the
#' same. Latent interactions (`XWITH`) are *not* random effects -- the
#' interaction term is an ordinary predictor -- so they are excluded here and
#' left in the output as a normal IV.
#'
#' A random *slope* is the special ON-form `s | y ON x`: its intercept/mean is
#' the between-person mean of a within-person effect, which [coef_table_mplus()]
#' re-expresses in the Within section of a two-level table. Growth factors
#' (e.g. `i s | y1-y4 AT a11-a14`) carry no `ON` relationship and so have no
#' outcome/predictor decomposition; they keep their own name.
#'
#' @param model Mplus model object from MplusAutomation.
#' @return A tibble with one row per declared random effect (one row per factor
#'   when several share a single `|` statement, e.g. `i s | ...`) and columns
#'   `slope` (upper-cased factor name), `definition` (the right-hand side, e.g.
#'   `"Y ON X"`), `outcome`/`predictor` (the DV/IV of an ON-form slope, `NA` for
#'   growth factors), and `kind` (`"slope"` for ON-form random slopes,
#'   `"growth"` otherwise). `XWITH` latent interactions are excluded. Empty
#'   tibble when no random effects are found.
#' @noRd
mplus_random_slopes <- function(model) {
  empty <- tibble(slope = character(0), definition = character(0),
                  outcome = character(0), predictor = character(0),
                  kind = character(0))
  modlines <- model$input$model
  if (is.null(modlines) || length(modlines) == 0) return(empty)
  modlines <- sub("!.*$", "", modlines)

  statements <- unlist(strsplit(paste(modlines, collapse = "\n"), ";"))
  bar_stmts  <- statements[str_detect(statements, "\\|")]
  if (length(bar_stmts) == 0) return(empty)

  map_dfr(bar_stmts, function(stmt) {
    parts <- strsplit(stmt, "\\|")[[1]]
    # Strip %WITHIN% / %BETWEEN% section headers that may share the chunk.
    lhs <- gsub("%[^%]*%", "", parts[1])
    rhs <- if (length(parts) >= 2) parts[2] else ""

    # Latent variable interactions are declared with `|` too (`fxg | f XWITH g`)
    # but the interaction term is a predictor, not a random effect: skip it.
    if (str_detect(toupper(rhs), "\\bXWITH\\b")) return(empty)

    # One `|` statement may name several factors (e.g. `i s | ...`).
    factors <- toupper(unlist(strsplit(trimws(lhs), "\\s+")))
    factors <- factors[nzchar(factors)]
    if (length(factors) == 0) return(empty)

    definition <- toupper(trimws(gsub("\\s+", " ", rhs)))
    if (str_detect(definition, "\\bON\\b")) {
      # Random slope: split the "Y ON X" relationship into outcome and predictor.
      outcome   <- trimws(sub("\\bON\\b.*$", "", definition))
      predictor <- trimws(sub("^.*\\bON\\b", "", definition))
      kind      <- "slope"
    } else {
      # Growth factor (e.g. `i s | y1-y4 AT a11-a14`): no ON decomposition.
      outcome   <- NA_character_
      predictor <- NA_character_
      kind      <- "growth"
    }
    tibble(slope = factors, definition = definition,
           outcome = outcome, predictor = predictor, kind = kind)
  })
}

#' Describe whether random-slope expectations are means or intercepts (internal)
#'
#' Mplus reports a random slope's expectation as an *intercept* when the slope
#' is regressed on a between-level predictor, and as a *mean* otherwise. This
#' lets [coef_table_mplus()] label the footnote with the term that actually
#' appears in the output rather than the generic "mean/intercept".
#'
#' @param model Mplus model object from MplusAutomation.
#' @return `"mean"`, `"intercept"`, or `"mean/intercept"` (when different slopes
#'   differ); `NA_character_` when no random-slope expectation is found.
#' @noRd
mplus_rs_expect_kind <- function(model) {
  rs     <- mplus_random_slopes(model)
  slopes <- rs$slope[rs$kind == "slope"]
  d <- model$parameters$unstandardized
  if (length(slopes) == 0 || is.null(d)) return(NA_character_)

  rows    <- d$paramHeader %in% c("Means", "Intercepts") & toupper(d$param) %in% slopes
  headers <- unique(d$paramHeader[rows])
  if (length(headers) == 0) return(NA_character_)

  kinds <- character(0)
  if ("Means" %in% headers)      kinds <- c(kinds, "mean")
  if ("Intercepts" %in% headers) kinds <- c(kinds, "intercept")
  paste(kinds, collapse = "/")
}

#' Detect whether a Mplus model appears to be a growth model (internal)
#'
#' The most direct clue available from parsed Mplus syntax is a `|` statement
#' without an `ON` relationship, e.g. `i s | y1-y4 AT ...`. `XWITH`
#' interactions are excluded by the random-slope parser.
#'
#' @param model Mplus model object from MplusAutomation.
#' @return Logical; TRUE when at least one growth-factor declaration is found.
#' @noRd
detect_mplus_growth <- function(model) {
  rs <- mplus_random_slopes(model)
  any(rs$kind == "growth")
}

#' Identify factor-like random effects for a growth table (internal)
#'
#' When a model is treated as growth, all latent variables declared with `|`
#' are shown in the growth-factor layout: ordinary growth factors (`kind =
#' "growth"`) and ON-form time-varying-covariate slopes (`kind = "slope"`).
#'
#' @param model Mplus model object from MplusAutomation.
#' @return Upper-case factor names, in syntax order.
#' @noRd
mplus_growth_factors <- function(model) {
  rs <- mplus_random_slopes(model)
  unique(rs$slope)
}

#' Extract Mplus confidence/credibility intervals robustly (internal)
#'
#' Intervals are joined back onto the estimates by display label, so the two
#' label sets must agree exactly. This helper prefers the intervals that Mplus
#' prints *in the main estimate table* (Bayesian credibility limits), because
#' those are reconstructed from the same `paramHeader`/`param` columns that
#' [stats::coef()] reads -- the labels are guaranteed to match.
#'
#' The dedicated `ci.*` tables that [stats::confint()] reads (Mplus CINTERVAL
#' output) are only used as a fallback, for frequentist models whose estimate
#' table carries no interval columns. Those tables are riskier as a join source:
#' Mplus truncates variable names to 8 characters in the CINTERVAL section, so a
#' long name (e.g. `M_APREG_S`) is printed as `M_APREG_` there but in full in
#' MODEL RESULTS, and the resulting labels no longer match.
#'
#' @param model Mplus model object from MplusAutomation.
#' @param params Parameter types to extract.
#' @param type Coefficient scale (`"un"`, `"stdyx"`, ...).
#' @return A tibble with `Label`, `LowerCI`, `UpperCI`, or `NULL` if no
#'   intervals are available.
#' @noRd
mplus_confint_safe <- function(model, params, type) {
  # Primary source: intervals printed inline in the estimate table. Same source
  # table as coef(), so labels always agree and long names are never truncated.
  direct <- mplus_confint_from_primary(model, params, type)
  if (!is.null(direct)) return(direct)

  # Fallback: the dedicated ci.* tables (CINTERVAL output), via confint().
  ci <- tryCatch(
    suppressWarnings(confint(model, params = params, type = type)),
    error = function(e) NULL
  )
  if (is.data.frame(ci) && nrow(ci) > 0 &&
      all(c("Label", "LowerCI", "UpperCI") %in% names(ci))) {
    return(tibble(Label = ci$Label, LowerCI = ci$LowerCI, UpperCI = ci$UpperCI))
  }
  NULL
}

#' Reconstruct intervals from the Mplus estimate table (internal)
#'
#' Returns `NULL` when the estimate table has no interval columns (e.g. most
#' frequentist output, where intervals live only in the CINTERVAL `ci.*` table).
#'
#' @inheritParams mplus_confint_safe
#' @return A tibble with `Label`, `LowerCI`, `UpperCI`, or `NULL`.
#' @noRd
mplus_confint_from_primary <- function(model, params, type) {
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

  # Match coef()/confint() label format exactly. MplusAutomation pastes the
  # section initial onto the label with a space separator, which leaves a
  # *leading space* for single-level models (empty section), e.g. " F4<-F3".
  bw <- if ("BetweenWithin" %in% names(d)) substr(d$BetweenWithin, 1, 1) else rep("", nrow(d))

  tibble(Label = paste(bw, label), LowerCI = d[[lo]], UpperCI = d[[hi]])
}

#' Identify mean/intercept/threshold (expectation) rows from the label (internal)
#'
#' Expectation rows must be detected from the Mplus parameter `Label`, not from
#' the `IV` column: `label_replace` rewrites `DV`/`IV` (and a user may legitimately
#' relabel the structural token `"Means"` to e.g. `"Mean / intercept"` for
#' display), but it never touches `Label`. Matching on `IV` would therefore
#' misclassify expectation rows whenever the user relabels these tokens, letting
#' a predictor-only variable's mean qualify it as a spurious outcome column.
#'
#' The label of an expectation row ends in `<-Means`, `<-Intercepts`, or
#' `<-Thresholds` (e.g. `"X<-Means"`). Re-expressed random-slope means share this
#' label shape (`"S<-Means"`) but are directed within-person effects, not
#' expectations; callers exclude them via the `within_avg_effect` flag.
#'
#' @param label Character vector of Mplus parameter labels.
#' @return Logical vector, `TRUE` for expectation rows.
#' @noRd
is_expectation_label <- function(label) {
  str_detect(label, "<-(Means|Intercepts|Thresholds)$")
}

#' Apply user label replacements without substring bleed (internal)
#'
#' `label_replace` maps raw Mplus variable names to display labels and is applied
#' with [stringr::str_replace_all()], whose keys are regular expressions matched
#' anywhere in the string. When one raw name is a prefix/substring of another
#' (e.g. `M_PA` and `M_PAW`), the shorter key would otherwise rewrite part of the
#' longer name and silently corrupt its label (`M_PAW` -> `<label for M_PA>W`),
#' so the relabelled variable no longer matches the original -- breaking, for
#' instance, `predictor_order` lookups and splitting a variable across labels.
#'
#' Replacements are therefore applied longest-key-first, so the most specific
#' (longest) names resolve before shorter ones can match inside them. Keys are
#' still treated as patterns, preserving intentional substring replacement for
#' non-overlapping keys.
#'
#' @param x Character vector to relabel.
#' @param label_replace Named character vector (`raw = display`), or `NULL`.
#' @return `x` with replacements applied; unchanged when `label_replace` is `NULL`.
#' @noRd
apply_label_replace <- function(x, label_replace) {
  if (is.null(label_replace) || length(label_replace) == 0) return(x)
  ord <- order(nchar(names(label_replace)), decreasing = TRUE)
  str_replace_all(x, label_replace[ord])
}

#' Format Mplus Model Coefficients
#'
#' Extracts regression (and optionally other) coefficients from an
#' MplusAutomation model and splits the Mplus parameter labels into dependent
#' (`DV`) and independent (`IV`) variable columns.
#'
#' The `|` operator in Mplus is overloaded: it declares random effects (random
#' intercepts/slopes in multilevel models), growth factors, and latent variable
#' interactions (`XWITH`). Random effects and growth factors are treated alike
#' (there is no reliable way to tell them apart). Latent interactions are *not*
#' random effects: the interaction term (e.g. `f1xf2` in `f1xf2 | f1 XWITH f2`)
#' is left in the output as an ordinary predictor.
#'
#' The multilevel annotations and random-slope re-expression below apply only to
#' two-level models. Single-level models -- including single-level growth models
#' and models with `XWITH` interactions -- are returned as plain coefficients,
#' with each random effect appearing under its own name as an ordinary
#' outcome/predictor.
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
#' * `display_level` -- a coarse classification of the parameter: `"Within"`
#'   (within-person effects, including random-slope means), `"Between"`
#'   (between-person effects), or `"Random effects"` (a random slope's
#'   cross-level predictors and variance). [coef_table_mplus()] uses this to
#'   route parameters: random-effect rows go to the separate Random effects
#'   table (with cross-level interactions also kept, relabelled, in the Between
#'   section of the main table).
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
#'   issued when the explicit value conflicts with the detected estimator.
#' @param addci Add confidence intervals to parameters
#' @param pval_note When `TRUE` (default) and the model is Bayesian, a message
#'   notes that the Mplus p-values are one-tailed. Set `FALSE` to suppress it
#'   (e.g. when significance is judged from credibility intervals rather than
#'   p-values, so the note is just noise). [coef_table_mplus()] sets this
#'   automatically based on whether it displays p-values.
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
coef_wrapper <- function(model, label_replace = NULL, params = c('regression'), type = "un", bayes = NULL, addci = FALSE, pval_note = TRUE) {
  detected_bayes <- detect_mplus_bayes(model)

  if (detected_bayes && isTRUE(pval_note)) {
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

  # Strip the multilevel section prefix ("B " / "W ") that the coef() method
  # prepends to two-level labels, recording the level it encodes.
  level <- dplyr::case_when(
    str_detect(testcoefs$Label, "^W ") ~ "Within",
    str_detect(testcoefs$Label, "^B ") ~ "Between",
    TRUE                               ~ NA_character_
  )
  testcoefs$Label <- stringr::str_remove(testcoefs$Label, "^[BW] ")

  testcoefs <- sep_label_mplus(testcoefs)

  # (Co)variance parameters use the "<->" operator, which sep_label_mplus does
  # not split cleanly; recover both sides directly from the label.
  und <- str_detect(testcoefs$Label, "<->")
  if (any(und)) {
    sides <- stringr::str_split_fixed(testcoefs$Label[und], "<->", 2)
    testcoefs$DV[und] <- sides[, 1]
    testcoefs$IV[und] <- sides[, 2]
  }

  twolevel <- is_mplus_twolevel(model)

  # The multilevel annotations and the random-slope re-expression below only
  # carry information for two-level models. Single-level models (including
  # single-level growth models and models with latent XWITH interactions) are
  # returned as plain coefficients: the random effects keep their own names and
  # appear as ordinary outcomes/predictors.
  if (twolevel) {
    # Classify parameters that involve a random slope (an ON-form latent slope
    # declared with `|`, e.g. `s | y ON x`). Growth factors and XWITH
    # interactions are excluded by mplus_random_slopes().
    slopes_tbl  <- mplus_random_slopes(model)
    slopes_tbl  <- slopes_tbl[slopes_tbl$kind == "slope", , drop = FALSE]
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
        level             = level,
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
  }

  if (!is.null(label_replace)) {
    testcoefs <- testcoefs |>
      mutate(DV = apply_label_replace(DV, label_replace),
             IV = apply_label_replace(IV, label_replace))
  }

  testcoefs
}

#' Build the standalone "Random effects" table (internal)
#'
#' Random slopes are associated with a specific within-person fixed effect, only
#' secondarily with an outcome variable, so they sit awkwardly in the predictor x
#' outcome layout of the main table. This helper collects every random-slope
#' parameter into its own table: per slope, its between-level predictors
#' (cross-level interactions), then its mean/intercept, then its (residual)
#' variance.
#'
#' The row identifier (`Parameter`) is derived from the Mplus parameter `Label`:
#' a slope mean/intercept becomes `"S Intercept"` / `"S Mean"`, its variance
#' becomes `"S Residual Variance"` / `"S Variance"`, and a cross-level predictor
#' keeps its raw label (e.g. `"S1<-W"`).
#'
#' @param re A tibble of the random-slope rows from [coef_wrapper()] (those with
#'   `random_slope == TRUE`), carrying `est`, `se`/`pval` or `LowerCI`/`UpperCI`,
#'   the `IV` column, and the `within_avg_effect`/`random_effect`/`variance` flags.
#' @param display `"est_ci"` or `"est_se"`.
#' @param effective_bayes Logical; controls credibility-vs-confidence wording.
#' @param sig_threshold P-value threshold for stars (est_se only).
#' @param digits Decimal places.
#' @return A `gt` table, or a plain tibble when **gt** is unavailable.
#' @noRd
mplus_random_effects_table <- function(re, display, effective_bayes,
                                       sig_threshold, digits,
                                       return_data = FALSE) {
  fmt <- function(x) format(round(x, digits), nsmall = digits)

  re <- re |>
    mutate(
      .slope = sub("(<->|<-).*$", "", Label),
      .ord   = dplyr::case_when(
        random_effect & !variance ~ 1L,  # cross-level predictors of the slope
        within_avg_effect         ~ 2L,  # the slope mean / intercept
        variance                  ~ 3L,  # the slope (residual) variance
        TRUE                      ~ 4L
      ),
      # Human-readable row label. Means/intercepts and variances are rewritten
      # in terms of the slope; cross-level predictors keep their raw Mplus label.
      Parameter = dplyr::case_when(
        within_avg_effect & grepl("Means$", Label)      ~ paste(.slope, "Mean"),
        within_avg_effect & grepl("Thresholds$", Label) ~ paste(.slope, "Threshold"),
        within_avg_effect                               ~ paste(.slope, "Intercept"),
        variance & grepl("residual", tolower(IV))       ~ paste(.slope, "Residual Variance"),
        variance                                        ~ paste(.slope, "Variance"),
        TRUE                                            ~ Label
      )
    ) |>
    dplyr::arrange(.slope, .ord)

  if (display == "est_ci") {
    out <- re |>
      mutate(
        .sig    = !(LowerCI <= 0 & UpperCI >= 0),
        est_col = paste0(fmt(est), if_else(.sig, "*", "")),
        ll_col  = fmt(LowerCI),
        ul_col  = fmt(UpperCI)
      ) |>
      select(Parameter, est_col, ll_col, ul_col)
    value_cols <- c("est_col", "ll_col", "ul_col")
    sub_labels <- c("Est.", "LL", "UL")
    interval   <- if (effective_bayes) "credibility" else "confidence"
    footnote   <- paste0(
      "* 95% ", interval, " interval excludes zero. ",
      "LL and UL are the lower and upper limits of the 95% ", interval, " interval."
    )
  } else {
    out <- re |>
      mutate(
        .sig    = !is.na(pval) & pval < sig_threshold,
        est_col = paste0(fmt(est), if_else(.sig, "*", "")),
        se_col  = fmt(se)
      ) |>
      select(Parameter, est_col, se_col)
    value_cols <- c("est_col", "se_col")
    sub_labels <- c("Est.", "SE")
    footnote   <- if (effective_bayes) {
      paste0("* p < ", sig_threshold, ", one-tailed.")
    } else {
      paste0("* p < ", sig_threshold)
    }
  }

  if (return_data || !requireNamespace("gt", quietly = TRUE)) return(out)

  col_labels_list <- stats::setNames(as.list(sub_labels), value_cols)
  col_labels_list[["Parameter"]] <- "Parameter"

  gt::gt(out) |>
    gt::cols_label(.list = col_labels_list) |>
    gt::tab_footnote(footnote = footnote) |>
    gt::tab_footnote(footnote = paste0(
      "Each random slope's between-level predictors (cross-level interactions), ",
      "its mean or intercept, and its (residual) variance. Cross-level ",
      "predictors keep their Mplus parameter label (latent slope name on the ",
      "left of the operator)."
    ))
}

#' Build a growth-factor coefficient table (internal)
#'
#' @param coefs Output of [coef_wrapper()] with regression, expectation and
#'   variability rows.
#' @param factor_names Growth/random-effect factor labels to display as outcome
#'   columns, after any label replacement.
#' @inheritParams mplus_random_effects_table
#' @inheritParams coef_table_mplus
#' @return A `gt` table, or a tibble when `return_data = TRUE` or **gt** is
#'   unavailable.
#' @noRd
mplus_growth_table <- function(coefs, factor_names, display, effective_bayes,
                               sig_threshold, digits, na_replace,
                               predictor_order = NULL,
                               return_data = FALSE) {
  fmt <- function(x) format(round(x, digits), nsmall = digits)

  coefs <- coefs |>
    mutate(DV = trimws(DV), IV = trimws(IV)) |>
    filter(DV %in% factor_names)

  factor_names <- intersect(factor_names, unique(coefs$DV))
  # Expectation/variance rows are classified from the label (see
  # is_expectation_label()), so user `label_replace` of structural tokens such as
  # "Means" does not misroute them.
  factor_regressed <- unique(coefs$DV[!is_expectation_label(coefs$Label) &
                                        !str_detect(coefs$Label, "<->")])

  coefs <- coefs |>
    mutate(
      .expectation = is_expectation_label(Label),
      .variance = str_detect(Label, "<->") & DV == IV,
      group = dplyr::case_when(
        .expectation ~ "Means",
        .variance    ~ "Variances",
        TRUE         ~ "Predictors"
      ),
      IV = dplyr::case_when(
        .expectation ~ "Mean",
        .variance & DV %in% factor_regressed ~ "Residual variance",
        .variance ~ "Variance",
        TRUE ~ IV
      )
    )

  if (display == "est_ci") {
    long <- coefs |>
      mutate(
        sig     = !(LowerCI <= 0 & UpperCI >= 0),
        est_col = paste0(fmt(est), if_else(sig, "*", "")),
        ll_col  = fmt(LowerCI),
        ul_col  = fmt(UpperCI)
      ) |>
      select(group, IV, DV, est_col, ll_col, ul_col)

    value_cols <- c("est_col", "ll_col", "ul_col")
    sub_labels <- c("Est.", "LL", "UL")
    interval   <- if (effective_bayes) "credibility" else "confidence"
    footnote   <- paste0(
      "* 95% ", interval, " interval excludes zero. ",
      "LL and UL are the lower and upper limits of the 95% ", interval, " interval."
    )
  } else {
    long <- coefs |>
      mutate(
        sig     = !is.na(pval) & pval < sig_threshold,
        est_col = paste0(fmt(est), if_else(sig, "*", "")),
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

  wide <- long |>
    pivot_wider(
      names_from  = DV,
      values_from = all_of(value_cols),
      names_glue  = "{.value}__{DV}"
    )

  ordered_cols <- c("group", "IV", unlist(lapply(factor_names, function(dv) {
    paste0(value_cols, "__", dv)
  })))
  wide <- wide |> select(all_of(ordered_cols))

  if (!is.null(na_replace)) {
    wide <- wide |>
      mutate(across(-all_of(c("group", "IV")), \(x) if_else(is.na(x), na_replace, x)))
  }

  if (!is.null(predictor_order)) {
    present <- unique(wide$IV)
    unknown <- setdiff(predictor_order, present)
    if (length(unknown) > 0) {
      warning("`predictor_order` lists predictors not in the table: ",
              paste(unknown, collapse = ", "), ".")
    }
    iv_levels <- c(intersect(predictor_order, present),
                   setdiff(present, predictor_order))
    wide <- wide |>
      mutate(IV = factor(IV, levels = iv_levels)) |>
      dplyr::arrange(group, IV) |>
      mutate(IV = as.character(IV))
  }

  group_order <- c("Predictors", "Means", "Variances")
  wide <- wide |>
    mutate(group = factor(group, levels = intersect(group_order, unique(group)))) |>
    dplyr::arrange(group) |>
    mutate(group = as.character(group))

  if (return_data || !requireNamespace("gt", quietly = TRUE)) return(wide)

  col_labels_list <- list(IV = "Parameter")
  for (dv in factor_names) {
    for (i in seq_along(value_cols)) {
      col_labels_list[[paste0(value_cols[i], "__", dv)]] <- sub_labels[i]
    }
  }

  gt_tbl <- gt::gt(wide, groupname_col = "group") |>
    gt::cols_label(.list = col_labels_list) |>
    gt::tab_footnote(footnote = footnote) |>
    gt::tab_footnote(footnote = paste0(
      "Growth-factor means and variances are shown in dedicated sections. ",
      "Residual variances are shown when a factor is regressed on a predictor."
    ))

  for (dv in factor_names) {
    gt_tbl <- gt_tbl |>
      gt::tab_spanner(label = dv, columns = paste0(value_cols, "__", dv))
  }

  gt_tbl |>
    gt::row_group_order(groups = intersect(group_order, unique(wide$group)))
}

#' Wide regression coefficient table from Mplus model
#'
#' Produces a predictor x outcome table with nested sub-columns for each outcome.
#' Display is controlled by `display_type`:
#' - `"est_se"`: estimate (starred when p < `sig_threshold`) and SE. When the
#'   model is Bayesian the footnote notes that p-values are one-tailed, and a
#'   message to the same effect is emitted (since p-values are in play here).
#' - `"est_ci"`: estimate (starred when the 95% CI/CrI excludes zero), and LL/UL,
#'   the lower and upper limits of that interval (defined in the footnote).
#'   Significance comes from the interval, not p-values, so the one-tailed
#'   p-value message is not emitted for Bayesian models in this mode.
#'
#' `display_type` is auto-detected from the estimator when `NULL` (default):
#' Bayesian models -> `"est_ci"`, all others -> `"est_se"`.
#' The estimator is auto-detected from the model when `bayes = NULL` (default).
#' Returns a `gt` table when the **gt** package is available, otherwise a plain tibble.
#'
#' ## Two-level models
#'
#' The main (fixed-effects) table is split into two sections (row groups in `gt`,
#' a leading `level` column in the plain tibble):
#' * **Within** -- within-person effects, including the means/intercepts of
#'   random slopes (latent slopes declared with `|`, e.g. `s | y ON x`). A slope
#'   mean is the between-person mean of a within-person effect and is shown under
#'   the predictor it represents (`x`), marked with a superscript letter.
#' * **Between** -- between-person effects on the real outcomes, including
#'   cross-level interactions (a slope regressed on a between predictor, `s ON
#'   w`). These are relabelled to name the interaction explicitly, e.g. the `w`
#'   predictor of `s2 | y2 ON y1` is shown as
#'   `"W (between) x Y1 (within)"` under outcome `Y2`.
#'
#' ## The Random effects table
#'
#' A random slope is associated with a specific within-person fixed effect and
#' only secondarily with an outcome, so its parameters do not fit cleanly into
#' the predictor x outcome layout. When the model has random slopes,
#' `coef_table_mplus()` therefore returns a **named list**
#' `list(fixed = <main table>, random = <Random effects table>)`. Models without
#' random slopes (single-level models, growth models, two-level models with no
#' `|` slope) return a single table as before.
#'
#' The Random effects table lists, for each random slope (grouped by the latent
#' slope, in the order its between-level predictors, then its mean/intercept,
#' then its (residual) variance), the estimate and either LL/UL or SE. It keeps
#' the Mplus parameter `Label` (slope name on the left of the operator) as the
#' row identifier. The slope mean and any cross-level predictors thus appear in
#' *both* tables: re-expressed as fixed effects in the main table, and
#' slope-by-slope here.
#'
#' For two-level models `params` defaults to
#' `c("regression", "expectation", "variability")` so means/intercepts and the
#' random-slope variance are included; variances that do not belong to a random
#' slope are omitted from both tables.
#'
#' ## Growth models
#'
#' When `model_type = "auto"` (default), models with a growth-factor `|`
#' declaration such as `i s | y1-y4 AT ...` are detected as growth models. Set
#' `model_type = "growth"` to force the growth layout, or `"default"` to use the
#' ordinary predictor x outcome layout. Growth tables show factor predictors,
#' means, and variances in separate row sections.
#'
#' Only genuine outcomes -- variables a predictor is regressed *onto* (the
#' dependent variable of a regression, a random-slope mean, or a cross-level
#' effect) -- are given an outcome column. A variable described only by its own
#' mean/intercept, variance, or covariance gets no column, even though Mplus
#' estimates a between-level mean for every latent component and variances are
#' often declared in the Within part for latent decomposition; all of its rows
#' are dropped so it cannot create a spurious column.
#'
#' @param model Mplus model object from MplusAutomation.
#' @param label_replace Named character vector for replacing DV/IV labels.
#' @param params Which parameter types to extract (passed to MplusAutomation).
#'   `NULL` (default) resolves to `c("regression", "expectation", "variability")`
#'   for two-level and growth tables, and `"regression"` otherwise.
#' @param bayes `NULL` (default) to auto-detect; `TRUE`/`FALSE` to override.
#'   Warnings from conflicting explicit values are issued by [coef_wrapper()].
#' @param display_type `NULL` (default, auto-detect), `"est_se"` (estimate + SE,
#'   p-value stars), or `"est_ci"` (estimate + CI LL/UL, interval-exclusion stars).
#' @param type coefficient scale passed through to [coef_wrapper()]. Default `"un"`.
#' @param sig_threshold P-value threshold for significance stars (`"est_se"` only, default 0.05).
#' @param digits Number of decimal places (default 3).
#' @param na_replace Character string to substitute for empty cells (predictors that
#'   do not enter a given outcome). Default `"-"`; set `NULL` to keep true `NA`
#'   values, which is usually preferable with `return_data = TRUE`.
#' @param predictor_order Character vector giving the order of the predictor
#'   (row) labels. Entries must match the *displayed* predictor labels, i.e.
#'   after any `label_replace` and random-slope remapping. Listed predictors come
#'   first, in the given order; any predictors not listed keep their original
#'   order after them. In two-level tables the order is applied within each
#'   section (Within / Between) of the main table. `NULL` (default) keeps the
#'   order in which predictors appear in the Mplus output. A warning is issued
#'   for listed names that are not present in the table.
#'
#' @param model_type `"auto"` (default), `"growth"`, or `"default"`. `"auto"`
#'   uses parsed `|` syntax to detect growth-factor models; `"growth"` forces
#'   the growth-specific table layout.
#' @param return_data `FALSE` (default) returns formatted `gt` tables when
#'   **gt** is installed. `TRUE` returns the underlying wide tibble(s), useful
#'   for data manipulation. Use `na_replace = NULL` with `return_data = TRUE` to
#'   keep missing cells as real `NA` values.
#'
#' @return A `gt` table (or a plain tibble when **gt** is not installed). For
#'   two-level models with random slopes, a named list with elements `fixed`
#'   (the main predictor x outcome table) and `random` (the Random effects
#'   table), each a `gt` table or tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' m <- MplusAutomation::readModels("inst/extdata/ex5.11.out")
#' coef_table_mplus(m)
#' coef_table_mplus(m, display_type = "est_ci")
#' coef_table_mplus(m, na_replace = NULL, return_data = TRUE)
#' coef_table_mplus(m, label_replace = c("F3" = "Mediator", "F4" = "Outcome"))
#' # put F2 above F1 in the predictor column
#' coef_table_mplus(m, predictor_order = c("F2", "F1"))
#' # two-level model with a random slope: returns list(fixed = , random = )
#' m2 <- MplusAutomation::readModels("inst/extdata/ex9.2c.out")
#' tbls <- coef_table_mplus(m2)
#' tbls$fixed   # within/between fixed effects (cross-level interactions relabelled)
#' tbls$random  # the Random effects table
#' }
coef_table_mplus <- function(model, label_replace = NULL, params = NULL,
                              bayes = NULL, display_type = NULL, type = "un",
                              sig_threshold = 0.05, digits = 3, na_replace = "-",
                              predictor_order = NULL,
                              model_type = c("auto", "growth", "default"),
                              return_data = FALSE) {

  model_type <- match.arg(model_type)

  if (!is.null(predictor_order) && !is.character(predictor_order)) {
    stop("`predictor_order` must be a character vector (or NULL).")
  }

  twolevel <- is_mplus_twolevel(model)
  growth_model <- model_type == "growth" ||
    (model_type == "auto" && detect_mplus_growth(model))

  if (is.null(params)) {
    params <- if (twolevel || growth_model) {
      c("regression", "expectation", "variability")
    } else {
      c("regression")
    }
  }

  # Resolve effective_bayes (silent); conflict warnings handled in coef_wrapper.
  effective_bayes <- if (is.null(bayes)) detect_mplus_bayes(model) else bayes

  effective_display <- if (is.null(display_type)) {
    if (effective_bayes) "est_ci" else "est_se"
  } else {
    display_type
  }

  fetch_ci <- effective_display == "est_ci"

  # The one-tailed p-value note is only relevant when p-values are actually
  # displayed (est_se). With est_ci significance comes from the credibility
  # interval, so the note would just be noise.
  coefs <- coef_wrapper(model, label_replace = label_replace, params = params,
                        type = type, bayes = bayes, addci = fetch_ci,
                        pval_note = effective_display == "est_se") |>
    filter(!is.na(DV)) |>
    mutate(DV = trimws(DV), IV = trimws(IV))

  # Drop variances that do not belong to a random slope (e.g. outcome residual
  # variances): only the random-slope (residual) variance is tabulated.
  if (twolevel) {
    coefs <- coefs |> filter(!(variance & !random_effect))
  }

  # A variable earns an outcome column only if a predictor is regressed *onto*
  # it -- a regression, a random-slope mean, or a cross-level effect. Parameters
  # that merely describe a variable's own distribution do not qualify it: a
  # mean/intercept, a variance, or a covariance is not a directed effect. This
  # matters because Mplus estimates a between-level mean for every latent
  # component, and variances are frequently declared in the Within part purely to
  # enable latent decomposition; a variable that is only a predictor (or only
  # decomposed) would otherwise gain a spurious outcome column carrying nothing
  # but its mean and/or variance. Both expectation and undirected ("x <-> y":
  # variances and covariances) rows are detected from the *label*, never from the
  # label-replaced `IV` column, so the rule holds even when the user relabels the
  # structural tokens (e.g. `label_replace = c("Means" = "Mean / intercept")`).
  is_expectation <- is_expectation_label(coefs$Label)
  is_undirected  <- str_detect(coefs$Label, "<->")

  # Re-expressed random-slope means carry a `<-Means`/`<-Intercepts` label but are
  # directed within-person effects (the average within effect of x on y), so they
  # must still qualify their outcome.
  if (twolevel) is_expectation <- is_expectation & !coefs$within_avg_effect

  if (growth_model) {
    # Growth/random-effect factors legitimately appear with only a mean and a
    # variance and no predictor; they are tabulated by mplus_growth_table() below.
    # Here only guard against a predictor-only variable gaining a column off a
    # bare mean.
    outcome_vars <- unique(coefs$DV[!is_expectation])
    coefs <- coefs[!(is_expectation & !coefs$DV %in% outcome_vars), , drop = FALSE]
  } else {
    # Drop every row of a variable that is never regressed onto, so it gets no
    # column (its mean/variance rows go with it). Real outcomes keep their
    # mean/variance rows because they qualify via their predictor(s).
    outcome_vars <- unique(coefs$DV[!is_expectation & !is_undirected])
    coefs <- coefs[coefs$DV %in% outcome_vars, , drop = FALSE]
  }

  # If intervals were requested but could not be retrieved, fall back to est/SE.
  if (fetch_ci && !all(c("LowerCI", "UpperCI") %in% names(coefs))) {
    warning("Intervals unavailable for this model; showing estimates and standard errors instead.")
    effective_display <- "est_se"
    fetch_ci <- FALSE
  }

  if (growth_model) {
    factor_names <- mplus_growth_factors(model)
    factor_names <- apply_label_replace(factor_names, label_replace)

    if (length(factor_names) == 0) {
      warning("No `|` growth/random-effect factors were detected; using the default table layout.")
      growth_model <- FALSE
    } else {
      return(mplus_growth_table(
        coefs = coefs,
        factor_names = factor_names,
        display = effective_display,
        effective_bayes = effective_bayes,
        sig_threshold = sig_threshold,
        digits = digits,
        na_replace = na_replace,
        predictor_order = predictor_order,
        return_data = return_data
      ))
    }
  }

  fmt <- function(x) format(round(x, digits), nsmall = digits)

  # Random slopes are tied to a within-person fixed effect, not primarily to an
  # outcome, so they get their own table rather than being squeezed into the
  # predictor x outcome layout. Build that table from the random-slope rows
  # before they are reshaped/dropped from the main table.
  has_rs     <- twolevel && any(coefs$random_slope)
  random_out <- NULL
  if (has_rs) {
    random_out <- mplus_random_effects_table(
      coefs |> filter(random_slope),
      effective_display, effective_bayes, sig_threshold, digits,
      return_data = return_data
    )
  }

  # Section column and random-slope-mean flag: present/meaningful only for
  # two-level models. `display_level`/`within_avg_effect`/`random_effect` exist
  # on `coefs` only when two-level, so guard the references.
  if (twolevel) {
    # A random slope's (residual) variance lives only in the Random effects
    # table; drop it from the main predictor x outcome table.
    coefs <- coefs |> filter(!variance)

    # Cross-level interactions (a slope regressed on a between predictor) remain
    # in the main table but are relabelled to name the interaction explicitly,
    # e.g. the predictor `W` of slope `s2 | y2 ON y1` becomes
    # `W (between) x Y1 (within)` under the `Y2` outcome. The within-person
    # partner comes from the slope definition.
    cli <- coefs$random_effect & !coefs$variance
    if (any(cli)) {
      rs <- mplus_random_slopes(model)
      rs <- rs[rs$kind == "slope", , drop = FALSE]
      pred_by_slope <- stats::setNames(rs$predictor, rs$slope)
      slope_nm <- sub("(<->|<-).*$", "", coefs$Label)
      pred     <- unname(pred_by_slope[slope_nm])
      pred <- apply_label_replace(pred, label_replace)
      coefs$IV[cli] <- paste0(coefs$IV[cli], " (between) x ", pred[cli], " (within)")
    }

    # Random-slope means stay in the Within section; everything else follows its
    # Mplus level (so cross-level interactions sit in Between with the other
    # between-level effects).
    coefs <- coefs |>
      mutate(group   = dplyr::if_else(within_avg_effect, "Within", level),
             rs_mean = within_avg_effect)
  } else {
    coefs <- coefs |> mutate(group = NA_character_, rs_mean = FALSE)
  }

  cli_cells <- if (twolevel) {
    coefs |>
      filter(random_effect, !variance) |>
      dplyr::distinct(group, IV, DV)
  } else {
    tibble(group = character(0), IV = character(0), DV = character(0))
  }

  rs_mean_cells <- if (twolevel) {
    coefs |>
      filter(rs_mean) |>
      dplyr::distinct(group, IV, DV)
  } else {
    tibble(group = character(0), IV = character(0), DV = character(0))
  }

  dvs <- unique(coefs$DV)

  if (effective_display == "est_ci") {
    long <- coefs |>
      mutate(
        sig     = !(LowerCI <= 0 & UpperCI >= 0),
        est_col = paste0(fmt(est), if_else(sig, "*", "")),
        ll_col  = fmt(LowerCI),
        ul_col  = fmt(UpperCI)
      ) |>
      select(group, IV, DV, est_col, ll_col, ul_col)

    value_cols <- c("est_col", "ll_col", "ul_col")
    sub_labels <- c("Est.", "LL", "UL")
    interval   <- if (effective_bayes) "credibility" else "confidence"
    footnote   <- paste0(
      "* 95% ", interval, " interval excludes zero. ",
      "LL and UL are the lower and upper limits of the 95% ", interval, " interval."
    )

  } else {
    long <- coefs |>
      mutate(
        sig     = !is.na(pval) & pval < sig_threshold,
        est_col = paste0(fmt(est), if_else(sig, "*", "")),
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

  # Optional explicit predictor (row) ordering. Listed predictors come first in
  # the given order; the rest keep their original order. Matches the displayed
  # IV labels (i.e. after label_replace / random-slope remapping). Encoded as a
  # factor so the section arrange can order predictors within each section.
  if (!is.null(predictor_order)) {
    present <- unique(wide$IV)
    unknown <- setdiff(predictor_order, present)
    if (length(unknown) > 0) {
      warning("`predictor_order` lists predictors not in the table: ",
              paste(unknown, collapse = ", "), ".")
    }
    iv_levels <- c(intersect(predictor_order, present),
                   setdiff(present, predictor_order))
    wide <- wide |> mutate(IV = factor(IV, levels = iv_levels))
  }

  # The main table now carries only Within and Between sections; random-slope
  # variances move to the Random effects table and cross-level interactions sit
  # in Between.
  section_order <- c("Within", "Between")

  if (twolevel) {
    # Order Within, Between. Within each section, follow predictor_order when
    # given, otherwise preserve the original order.
    section_levels <- intersect(section_order, unique(wide$group))
    wide <- wide |> mutate(group = factor(group, levels = section_levels))
    wide <- if (is.null(predictor_order)) {
      wide |> dplyr::arrange(group)
    } else {
      wide |> dplyr::arrange(group, IV)
    }
    wide <- wide |>
      mutate(group = as.character(group)) |>
      dplyr::rename(level = group)
  } else if (!is.null(predictor_order)) {
    wide <- wide |> dplyr::arrange(IV) |> select(-all_of("group"))
  } else {
    wide <- wide |> select(-all_of("group"))
  }

  if (!is.null(predictor_order)) {
    wide <- wide |> mutate(IV = as.character(IV))
  }

  if (return_data || !requireNamespace("gt", quietly = TRUE)) {
    if (has_rs) return(list(fixed = wide, random = random_out))
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
    gt::tab_footnote(footnote = footnote) |>
    gt::opt_footnote_marks(marks = "letters")

  # One spanner per outcome.
  for (dv in dvs) {
    gt_tbl <- gt_tbl |>
      gt::tab_spanner(label = dv, columns = paste0(value_cols, "__", dv))
  }

  if (twolevel) {
    section_levels <- intersect(section_order, unique(wide$level))
    gt_tbl <- gt_tbl |> gt::row_group_order(groups = section_levels)

    # Note marking random-slope means/intercepts; the rest are fixed effects.
    if (isTRUE(any(coefs$rs_mean))) {
      kind <- mplus_rs_expect_kind(model)
      if (is.na(kind)) kind <- "mean/intercept"
      rs_note <- paste0(
        "Random slope ", kind, ". ",
        "Unless otherwise noted, within-person coefficients have been ",
        "modelled as fixed effects."
      )
      for (i in seq_len(nrow(rs_mean_cells))) {
        cell_col <- paste0("est_col__", rs_mean_cells$DV[i])
        cell_row <- which(
          wide$level == rs_mean_cells$group[i] &
            wide$IV == rs_mean_cells$IV[i]
        )
        gt_tbl <- gt_tbl |>
          gt::tab_footnote(
            footnote = rs_note,
            locations = gt::cells_body(
              columns = all_of(cell_col),
              rows = cell_row
            )
          )
      }
    }

    # Point readers to the companion Random effects table from each cross-level
    # interaction estimate cell.
    if (has_rs) {
      cli_note <- paste0(
        "Cross-level interactions are shown as \"<between predictor> (between) x ",
        "<within predictor> (within)\". Each random slope's mean, variance ",
        "and predictors are also listed in the accompanying Random effects table."
      )
      for (i in seq_len(nrow(cli_cells))) {
        cell_col <- paste0("est_col__", cli_cells$DV[i])
        cell_row <- which(
          wide$level == cli_cells$group[i] &
            wide$IV == cli_cells$IV[i]
        )
        gt_tbl <- gt_tbl |>
          gt::tab_footnote(
            footnote = cli_note,
            locations = gt::cells_body(
              columns = all_of(cell_col),
              rows = cell_row
            )
          )
      }
    }
  }

  if (has_rs) return(list(fixed = gt_tbl, random = random_out))
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
