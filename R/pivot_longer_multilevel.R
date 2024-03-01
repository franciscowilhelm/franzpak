#' Gather wide two-level data into long format
#' Specifically designed for two-level data of repeated measures. Will create NAs for missing time points.
#' @param data dataframe containing two-level data in wide format
#' @param varlist list of variables to be pivoted into longer format
#' @param maxlevel1 specify the maximum number of level 1 measurements per cluster.
#' @param varnames optionally specify the names of the new variables, otherwise names are taken from varlist.
#'
#' @return a tibble with id (identifying the level-2 unit), time (identify the ordered level-1 unit), and further columns as specified in varlist (or varnames, if given).
#' @export
#' @details
#' The function assumes that the sequence of level 1 measurements is ordered and meaningful, as is the case with repeated measurements / diary designs.
#' This contrasts with multilevel data gathered from groups (e.g., organizations) in which the sequence of level 1 meaurements is unordered and incidental.
#' However, the function may still be useful for data with unordered level-1 measurements.
#' The maximum number of rows of the returned tibble is id (number of level-2 observations) x maxlevlel1 (maximum number of level-1 observations. Completely NA level-1 observations are removed, which may result in a reduced number of rows.
#'
#' @examples
#' # generate wide dataset with 10 clusters, x measured three times and y measured three times
#' df_twolevel_wide <- tibble::tibble(id = 1:10, x_t1 = rep(1:5,2),
#'   x_t2 = rep(1:5,2), x_t3 = rep(1:5,2),
#'   y_t1 = rep(1:5,2), y_t2 = rep(1:5,2), y_t3 = rep(1:5,2))
#' pivot_longer_multilevel(df_twolevel_wide, c("x", "y"), 3)

pivot_longer_multilevel <- function(data, varlist, maxlevel1, varnames = NULL) {
  # first, gather all the scale scores separately.
  gatherlist <- purrr::map(varlist, function(x) data |>
                      dplyr::select(tidyselect::contains(x))  |>
                      mutate(id = dplyr::row_number()) |>
                      tidyr::pivot_longer(cols = -id, names_to = "time", values_to = "value") |>
                      dplyr::arrange(id))

  # then, set up the variables for a rearranged dataframe (id,time, value)
  id <- gatherlist[[1]]$id
  time <- rep(0:(maxlevel1-1), length(unique(id)))
  scalevalue <- purrr::map2_dfc(gatherlist, varlist, function(x, name) {
    out <- x |> dplyr::select(value)
    names(out) <- name
    out
  })
  # id should be of equal length as time
  if(length(id) != length(time)) {
    rlang::abort("Could not pivot into long format. Check your selection of variables or your argument to maxlevel1")
  }

  if(missing(varnames)) {
    varnames <- varlist
    names(scalevalue) <- varnames
  } else {
    names(scalevalue) <- varnames
  }
  out <- dplyr::bind_cols(tibble::tibble(id = id, time = time), scalevalue)

  # prune level-1 measurements that are completely NA
  check_all_na <- function(row, cols) {
    all(is.na(row[cols]))
  }
  idx <- apply(out[, varnames, drop = FALSE], 1, check_all_na, cols = varnames)
  out <- out[!idx,]

  return(out)
}
