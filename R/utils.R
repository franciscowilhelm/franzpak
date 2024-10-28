#' Assign significance signs following APA nomenclature
#'
#' @param x numeric vector to be annotated using significance signs.
#'
#' @return character vector of significance signs
# #' @export # do not export
#'
#' @noRd
star_assign <- function(x, use.001 = TRUE) {
  if(!is.na(x)) {
    if(use.001 == TRUE) {
      if(x < 0.001) "***"
      else if (x < 0.01) "**"
      else if (x < 0.05) "*"
      else ""
    } else {
      if (x < 0.01) "**"
      else if (x < 0.05) "*"
      else ""
    }

  }
  else ""
}
vstar_assign <- Vectorize(star_assign)

