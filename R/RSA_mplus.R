#' Plot RSA surfaces from Mplus output
#'
#' @param model Path to an Mplus `.out` file or an object returned by
#'   [MplusAutomation::readModels()].
#' @param outcome Dependent variable label from the Mplus regression table.
#' @param pred_x Label of the linear X predictor in the Mplus output.
#' @param pred_y Label of the linear Y predictor in the Mplus output.
#' @param pred_x2 Label of the squared X term in the Mplus output.
#' @param pred_xy Label of the XY interaction term in the Mplus output.
#' @param pred_y2 Label of the squared Y term in the Mplus output.
#' @param b0 Optional intercept passed to [RSA::plotRSA()]. If `NULL`, the
#'   function looks for `<outcome><-Intercepts>` in the Mplus expectation
#'   parameters and otherwise falls back to `0`.
#' @param coef_type Which Mplus coefficient table to use. Passed to
#'   [stats::coef()] for `mplus.model` objects.
#' @param new_labels Optional character vector of `NEW` parameter labels to
#'   return. Matching is case-insensitive.
#' @param include_new If `TRUE`, include `NEW` parameters from `MODEL
#'   CONSTRAINT` in the returned object.
#' @param plot If `TRUE`, call [RSA::plotRSA()]. If `FALSE`, only return the
#'   extracted coefficients and metadata.
#' @param xlab Optional x-axis label passed to [RSA::plotRSA()]. Defaults to
#'   `pred_x`.
#' @param ylab Optional y-axis label passed to [RSA::plotRSA()]. Defaults to
#'   `pred_y`.
#' @param zlab Optional z-axis label passed to [RSA::plotRSA()]. Defaults to
#'   `outcome`.
#' @param ... Additional arguments passed to [RSA::plotRSA()].
#'
#' @return A list with the extracted RSA coefficients, selected Mplus
#'   parameters, optional `NEW` parameters, and the plot object when
#'   `plot = TRUE`.
#' @details `RSA::plotRSA()` accepts the polynomial coefficients directly
#'   (`x`, `y`, `x2`, `xy`, `y2`, and optionally `b0`). `NEW` parameters from
#'   `MODEL CONSTRAINT` are returned for inspection, but are not passed to
#'   `plotRSA()` because that function computes surface parameters internally
#'   from the polynomial coefficients.
#' @export
#'
#' @examples
#' \dontrun{
#' rsa_pos <- RSA_mplus(
#'   system.file("extdata", "congruence_sim.out", package = "franzpak"),
#'   outcome = "Z",
#'   pred_x = "X",
#'   pred_y = "Y",
#'   pred_x2 = "XS",
#'   pred_xy = "XY",
#'   pred_y2 = "YS",
#'   plot = TRUE
#' )
#' }
RSA_mplus <- function(model,
                      outcome,
                      pred_x,
                      pred_y,
                      pred_x2,
                      pred_xy,
                      pred_y2,
                      b0 = NULL,
                      coef_type = c("un", "std", "stdy", "stdyx"),
                      new_labels = NULL,
                      include_new = TRUE,
                      plot = TRUE,
                      xlab = NULL,
                      ylab = NULL,
                      zlab = NULL,
                      ...) {
  normalize_label <- function(x) {
    toupper(trimws(x))
  }

  match_rows <- function(parameters, labels, context) {
    parameters <- parameters
    parameters$Label <- normalize_label(parameters$Label)

    matched_rows <- lapply(labels, function(label) {
      hits <- which(parameters$Label == normalize_label(label))

      if (length(hits) == 0L) {
        rlang::abort(sprintf("Could not find `%s` in the Mplus %s.", label, context))
      }

      if (length(hits) > 1L) {
        rlang::abort(sprintf("Found multiple matches for `%s` in the Mplus %s.", label, context))
      }

      parameters[hits, , drop = FALSE]
    })

    matched_rows <- do.call(rbind, matched_rows)
    matched_rows$term <- names(labels)
    rownames(matched_rows) <- NULL
    matched_rows
  }

  extract_intercept <- function(model, outcome, coef_type) {
    expectation_parameters <- tryCatch(
      as.data.frame(
        stats::coef(model, type = coef_type, params = "expectation"),
        stringsAsFactors = FALSE
      ),
      error = function(e) NULL
    )

    if (is.null(expectation_parameters) || nrow(expectation_parameters) == 0L) {
      return(NA_real_)
    }

    expectation_parameters$Label <- normalize_label(expectation_parameters$Label)
    intercept_label <- paste0(outcome, "<-Intercepts")
    hits <- which(expectation_parameters$Label == normalize_label(intercept_label))

    if (length(hits) == 0L) {
      return(NA_real_)
    }

    expectation_parameters$est[hits[[1]]]
  }

  extract_new <- function(model, coef_type, new_labels = NULL) {
    new_parameters <- tryCatch(
      as.data.frame(
        stats::coef(model, type = coef_type, params = "new"),
        stringsAsFactors = FALSE
      ),
      error = function(e) NULL
    )

    if (is.null(new_parameters) || nrow(new_parameters) == 0L) {
      return(NULL)
    }

    new_parameters$Label <- normalize_label(new_parameters$Label)

    if (is.null(new_labels)) {
      rownames(new_parameters) <- NULL
      return(new_parameters)
    }

    match_rows(
      parameters = new_parameters,
      labels = stats::setNames(new_labels, new_labels),
      context = "NEW parameters"
    )
  }

  if (inherits(model, "mplus.model")) {
    model <- model
  } else {
    if (!is.character(model) || length(model) != 1L || is.na(model)) {
      rlang::abort("`model` must be a single file path or an `mplus.model` object.")
    }

    if (!requireNamespace("MplusAutomation", quietly = TRUE)) {
      rlang::abort("Package `MplusAutomation` must be installed to read Mplus output files.")
    }

    model <- MplusAutomation::readModels(target = model, quiet = TRUE)
  }

  coef_type <- match.arg(coef_type)

  regression_parameters <- as.data.frame(
    stats::coef(model, type = coef_type, params = "regression"),
    stringsAsFactors = FALSE
  )

  if (nrow(regression_parameters) == 0L) {
    rlang::abort("No regression parameters were found in the Mplus output.")
  }

  requested_labels <- c(
    x = paste0(outcome, "<-", pred_x),
    y = paste0(outcome, "<-", pred_y),
    x2 = paste0(outcome, "<-", pred_x2),
    xy = paste0(outcome, "<-", pred_xy),
    y2 = paste0(outcome, "<-", pred_y2)
  )

  matched_regressions <- match_rows(
    regression_parameters,
    requested_labels,
    context = "regression parameters"
  )

  coefficients <- stats::setNames(matched_regressions$est, names(requested_labels))

  if (is.null(b0)) {
    b0 <- extract_intercept(model, outcome = outcome, coef_type = coef_type)

    if (is.na(b0)) {
      b0 <- 0
      warning(
        sprintf(
          "No intercept for outcome `%s` was found in the Mplus expectation parameters; using `b0 = 0`.",
          outcome
        ),
        call. = FALSE
      )
    }
  }

  coefficients <- c(coefficients, b0 = b0)

  new_parameters <- NULL
  if (isTRUE(include_new)) {
    new_parameters <- extract_new(
      model = model,
      coef_type = coef_type,
      new_labels = new_labels
    )
  }

  plot_object <- NULL
  if (isTRUE(plot)) {
    if (!requireNamespace("RSA", quietly = TRUE)) {
      rlang::abort("Package `RSA` must be installed when `plot = TRUE`.")
    }

    if (is.null(xlab)) {
      xlab <- pred_x
    }
    if (is.null(ylab)) {
      ylab <- pred_y
    }
    if (is.null(zlab)) {
      zlab <- outcome
    }

    plot_args <- c(
      list(
        x = unname(coefficients["x"]),
        y = unname(coefficients["y"]),
        x2 = unname(coefficients["x2"]),
        y2 = unname(coefficients["y2"]),
        xy = unname(coefficients["xy"]),
        b0 = unname(coefficients["b0"]),
        xlab = xlab,
        ylab = ylab,
        zlab = zlab
      ),
      list(...)
    )

    plot_object <- do.call(RSA::plotRSA, plot_args)
  }

  out <- list(
    plot = plot_object,
    coefficients = coefficients,
    regression_parameters = matched_regressions,
    new_parameters = new_parameters,
    model = model,
    outcome = outcome,
    coefficient_type = coef_type
  )

  class(out) <- "rsa_mplus"
  out
}
