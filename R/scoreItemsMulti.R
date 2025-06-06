#' Automatically score multiple scales and return several psychometrical scale parameters
#'
#' This is a convenience wrapper for [psych::scoreItems()] that allows automatically
#' scoring multiple scales at once.
#' @param scalenames character vector of scale names
#' @param dataframe dataframe holding the items to be scored
#' @param exclude Boolean indicating whether to exclude participant responses where more than 1/3 of a scale are NA.
#' @param manual_keys named list with manual keys, formatted like in [psych::scoreItems()].
#' @param ... additional arguments to be passed to [psych::scoreItems()].
#' @return a list object holding scale scores and other information.
#' @export
#' @details
#' Note that, in line with the scoreItems function, scalenames is the first argument and data is the second argument.
#' scoreItemsMulti always uses impute = "none" instead of the default impute="median" for [psych::scoreItems()].
#' @examples
#' scalenames <- c("firstscale", "secondscale")
#' scoreItemsMulti(scalenames, mc_items) # automatically detect reverse coded items
#' # manually reverse items
#' scoreItemsMulti(scalenames, mc_items,
#'                 manual_keys = list(
#'                   secondscale = c(
#'                     "secondscale_1",
#'                     "secondscale_2",
#'                     "secondscale_3",
#'                     "secondscale_4",
#'                     "-secondscale_5"
#'                   )
#'                 ))

scoreItemsMulti <- function(scalenames, dataframe, exclude = TRUE, manual_keys = NULL, ...) {



  purrr::walk(scalenames,
       function(x) {
         tmp <- dplyr::select(dataframe, starts_with(x))
         if (ncol(tmp) == 0) {
           stop(
             "For one or more scalenames no items could be found. Please check your scalenames/dataframe."
           )
         }
       } )

  if(!is.null(manual_keys)) {
    if (length(intersect(scalenames, names(manual_keys))) != length(names(manual_keys))) {
      stop(
        c(
          "For one or more manual_keys no matching scalename could be found. Possibly there is a typo in the manual_keys or scalenames.
           Following scales in the manual_keys have no equivalent in the scalenames: ",
          names(manual_keys)[!(names(manual_keys) %in% scalenames)]
        )
      )
    }
  }


  # select items from dataframe
  dataframe_items <-  dplyr::select(dataframe, starts_with(scalenames))


  exclude_helper <- function(scale) {
           # if exclude = TRUE, exclude all persons who have more than 1/3 NA
           max_na <- ncol(scale) / 3
           real_na <- apply(scale, 1, function(x)
             sum(is.na(x)))
           index_na <- (real_na > max_na)
           scale[index_na, ] <- NA
           return(scale)
  }

  # modify dataframe such that participants who have to many NAs dont get scores
  dataframe_exclude <- purrr::map_dfc(scalenames,
                                             function(x) {
                                               exclude_helper(scale = dplyr::select(dataframe_items, dplyr::starts_with(x)))
                                             })
  scalenames_autoonly <- setdiff(scalenames, names(manual_keys)) # only auto generate keys for those where no manual keys are provided.

  keys.list <-
    purrr::map(scalenames_autoonly, function(x) {
      names(dplyr::select(dataframe_items, dplyr::starts_with(x)))
    })
  names(keys.list) <- scalenames_autoonly

  negativeitems <- function(scale) {
    psych::pca(scale)$loadings < 0
  }

  negative_index <-
    purrr::map(scalenames_autoonly, function(x) {
      negativeitems(dplyr::select(dataframe_items, dplyr::starts_with(x)))
    })
  names(negative_index) <- scalenames_autoonly

  # Identify scales with negatively correlated items
  scales_with_negatives <- names(negative_index)[purrr::map_lgl(negative_index, any)]

  if (length(scales_with_negatives) > 0) {
    message(
      paste0(
        "Some items were negatively correlated with total scale(s) and were (automatically) reversed. \n",
        "Scales with reversed items: ", paste(scales_with_negatives, collapse = ", "), "\n",
        "Please Check $negative_index for details."
      )
    )
  }


  keys_negative <-
    purrr::map2(keys.list, negative_index, function(x, y) {
      x[y] <- stringr::str_c("-", x[y], sep = "")
      return(x)
    })


  # manual keys
  if (!is.null(manual_keys)) {
    for (i in seq_along(manual_keys)) {
      keys_negative[[names(manual_keys[i])]] <- manual_keys[[i]]
      # create negative index based on manual inputs
      negative_index[[names(manual_keys[i])]] <- ifelse(grepl("-", manual_keys[[i]], fixed = TRUE), TRUE, FALSE)
    }
  }


    list_scored <-
      purrr::map(scalenames, function(name) {
        keys <- keys_negative[[name]]
        if (exclude == TRUE) {
          subdf <- dataframe_exclude |> dplyr::select(starts_with(name))
        } else { subdf <- dataframe_items |> dplyr::select(starts_with(name)) }
        out <- psych::scoreItems(keys = keys, subdf, impute = "none", ...)
        out$scores[] <-
          apply(out$scores, 2, function(a) {
            ifelse(is.nan(a), NA_real_, a)
          })
        return(out)
      })

  names(list_scored) <- scalenames
  scores <- purrr::map2_dfc(list_scored, scalenames, function(scale, name) {
    x <- data.frame(scale$scores)
    names(x) <- name
    return(x)
  })
  alpha <-   purrr::map2_dfc(list_scored, scalenames, function(scale, name) {
    x <- data.frame(scale$alpha)
    names(x) <- name
    return(x)
  })

  scaleout <- list(scores = scores, alpha = alpha, negative_index = negative_index)


  return(scaleout)
}
