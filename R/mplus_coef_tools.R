#' Separate Mplus output labels into X and Y (internal function)
#'
#' @param model mplus model as input.
#'
#' @return dataframe with coefficients and DV (dependent variable) and IV (independent variable) labels.
#' @export
#'
#' @examples
#' @noRd
sep_label <- function(model) {
  # takes coef() as input
  modelcoefs <- coef(model)
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

#' Format Mplus Model Coefficients
#'
#' @param model the Mplus model from MplusAutomation.
#' @param label_replace named character vector for replacing parameter labels
#' @param params which types of parameters to extract, see MplusAutomation
#' @param bayes Double p-value for one-tailed Bayes tests (gives rough p-estimates)
#' @param addci Add confidence intervals to parameters
#'
#' @return a dataframe with mplus model coefficients
#' @export
#'
#' @examples
coef_wrapper <- function(model, label_replace = NULL, params = c('regression'), bayes = FALSE, addci = FALSE) {
  # get coefs
  testcoefs <-
    coef(model, params = params)
  # make p values two tailed (rough) if Bayes
  if(bayes == TRUE) {
    testcoefs <-  testcoefs |> mutate(pval = pval*2)
  }
  # add Confidence intervals
  if(addci == TRUE) {
    testcoefs <- left_join(testcoefs, confint(model,params), by = "Label")

  }
  # add DV and IV and replace labels
  testcoefs <- sep_label(testcoefs)
  if(!is.null(label_replace)) {
    testcoefs <-
      testcoefs |> mutate(DV = str_replace_all(DV, label_replace),
                          IV = str_replace_all(IV, label_replace))
  }


  # testcoefs <- bind_rows(testcoefs, coef(model, params = 'new') |> select(Label, est, se, pval) |> mutate(IV = str_replace_all(Label, label_replace)))
  testcoefs
}

#' In beta/questioning status.
#'
#' @param model
#'
#' @return
#' @export
#'
#' @examples
confint_wrapper <- function(model) {
  message("Experimental function, may be deprecated in favor coef_wrapper()")
  coefs <- coef(model, params = 'new') |> select(Label, est)
  confints <- confint(model, params = c('new')) |> mutate(IV = str_replace_all(Label, label_replace))
  left_join(coefs, confints, by = "Label")
}
