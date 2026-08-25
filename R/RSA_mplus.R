.rsahelpers_available <- function() {
  requireNamespace("rsahelpers", quietly = TRUE)
}

#' Deprecated Mplus RSA helper
#'
#' `RSA_mplus()` has moved to the `rsahelpers` package. This compatibility
#' wrapper warns about the move and forwards all arguments to
#' [rsahelpers::RSA_mplus()].
#'
#' @param model Path to an Mplus `.out` file or an object returned by
#'   [MplusAutomation::readModels()].
#' @param outcome Dependent variable label from the Mplus regression table.
#' @param pred_x Label of the linear X predictor in the Mplus output.
#' @param pred_y Label of the linear Y predictor in the Mplus output.
#' @param pred_x2 Label of the squared X term in the Mplus output.
#' @param pred_xy Label of the XY interaction term in the Mplus output.
#' @param pred_y2 Label of the squared Y term in the Mplus output.
#' @param b0 Optional intercept forwarded to [rsahelpers::RSA_mplus()].
#' @param coef_type Which Mplus coefficient table to use.
#' @param new_labels Optional character vector of `NEW` parameter labels to
#'   return.
#' @param include_new If `TRUE`, include `NEW` parameters from `MODEL
#'   CONSTRAINT` in the returned object.
#' @param plot If `TRUE`, plot the extracted polynomial response surface.
#' @param xlab,ylab,zlab Optional axis labels passed to the plotting method.
#' @param ... Additional arguments passed to [rsahelpers::RSA_mplus()].
#'
#' @return The value returned by [rsahelpers::RSA_mplus()].
#' @export
#'
#' @examples
#' \dontrun{
#' RSA_mplus(
#'   "model.out",
#'   outcome = "Z",
#'   pred_x = "X",
#'   pred_y = "Y",
#'   pred_x2 = "XS",
#'   pred_xy = "XY",
#'   pred_y2 = "YS"
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
  .Deprecated(
    new = "rsahelpers::RSA_mplus",
    package = "franzpak",
    old = "franzpak::RSA_mplus"
  )

  if (!.rsahelpers_available()) {
    rlang::abort(c(
      "Package `rsahelpers` is required to use `franzpak::RSA_mplus()`.",
      "i" = "Install it with `pak::pak(\"franciscowilhelm/rsahelpers\")`."
    ))
  }

  rsahelpers::RSA_mplus(
    model = model,
    outcome = outcome,
    pred_x = pred_x,
    pred_y = pred_y,
    pred_x2 = pred_x2,
    pred_xy = pred_xy,
    pred_y2 = pred_y2,
    b0 = b0,
    coef_type = coef_type,
    new_labels = new_labels,
    include_new = include_new,
    plot = plot,
    xlab = xlab,
    ylab = ylab,
    zlab = zlab,
    ...
  )
}
