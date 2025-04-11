#' Automatically score multiple scales using scoreFast
#'
#' This is a convenience wrapper for [psych::scoreFast()] that allows automatically
#' scoring multiple scales at once. It is designed for speed and for cases
#' with few observations where PCA or reliability estimation might fail.
#' Unlike `scoreItemsMulti`, this function does *not* calculate Cronbach's alpha
#' or perform PCA. By default, it assumes no items are reverse-coded unless
#' `manual_keys` are provided.
#'
#' @param scalenames character vector of scale names
#' @param dataframe dataframe holding the items to be scored
#' @param manual_keys Optional. A named list with manual keys for scales, formatted
#'   like in [psych::scoreItems()] or [psych::scoreFast()]. Items to be reversed
#'   must be prefixed with "-". If provided, keys must be supplied for *all*
#'   scales listed in `scalenames`. If `NULL` (the default), the function attempts
#'   to find items based on naming convention (items starting with `scalename` followed
#'   by `.` or `_`) and assumes **no items are reverse-coded**.
#' @param exclude Boolean indicating whether to exclude participant responses where
#'   more than 1/3 of a scale's items (as determined by the keys used) are NA.
#'   Defaults to TRUE.
#' @param ... additional arguments to be passed to [psych::scoreFast()].
#' @return a list object holding:
#'   \item{scores}{A dataframe containing the mean score for each scale.}
#'   \item{negative_index}{A list indicating which items (by position within the
#'         key list for that scale) were treated as reversed. This is based on
#'         the provided `manual_keys` if supplied, otherwise all are `FALSE`.}
#' @export
#' @details
#' `scoreItemsMultiFast` uses `psych::scoreFast` under the hood.
#'
#' If `manual_keys` is provided, it behaves like the original function, requiring
#' keys for all `scalenames` and respecting the "-" prefix for reverse coding.
#'
#' If `manual_keys` is `NULL` (default):
#'   * A **warning** is issued reminding the user that no automatic reverse
#'     coding detection occurs.
#'   * The function attempts to identify items for each scale by looking for
#'     column names in `dataframe` that start with the `scalename` followed by
#'     either a period (`.`) or an underscore (`_`). For example, for `scalename = "myScale"`,
#'     it would look for items like `myScale.1`, `myScale_A`, etc.
#'   * It assumes **none** of these automatically identified items are reverse-coded.
#'   * If no items matching the naming convention are found for a given scale, an error occurs.
#'
#' It uses `missing = FALSE` (no imputation) and `totals = FALSE` (calculates mean scores)
#' as defaults passed to `scoreFast`, but these can be overridden via `...`.
#'
#' @examples
#' # Create some dummy data
#' set.seed(123)
#' mc_items_fast <- data.frame(
#'  firstscale.1 = sample(1:5, 20, replace = TRUE),
#'  firstscale.2 = sample(1:5, 20, replace = TRUE),
#'  firstscale_3r = sample(1:5, 20, replace = TRUE), # conceptually reversed
#'  secondscale_1 = sample(1:7, 20, replace = TRUE),
#'  secondscale_2r = sample(1:7, 20, replace = TRUE), # conceptually reversed
#'  secondscale_3 = sample(1:7, 20, replace = TRUE),
#'  unrelated_item = rnorm(20)
#' )
#' # Add some NAs to test exclude
#' mc_items_fast[1, 1:2] <- NA # More than 1/3 NA for firstscale for P1 (2 out of 3)
#' mc_items_fast[2, 4] <- NA   # Less than 1/3 NA for secondscale for P2 (1 out of 3)
#'
#' scalenames_fast <- c("firstscale", "secondscale")
#'
#' # --- Example 1: Providing manual keys (required explicit reversal) ---
#' manual_keys_fast <- list(
#'   firstscale = c("firstscale.1", "firstscale.2", "-firstscale_3r"),
#'   secondscale = c("secondscale_1", "-secondscale_2r", "secondscale_3")
#' )
#'
#' scored_data_manual <- scoreItemsMultiFast(
#'   scalenames = scalenames_fast,
#'   dataframe = mc_items_fast,
#'   manual_keys = manual_keys_fast,
#'   exclude = TRUE
#' )
#'
#' print("Scores (Manual Keys, exclude = TRUE):")
#' print(scored_data_manual$scores)
#' print("Negative Index (Manual Keys):")
#' print(scored_data_manual$negative_index)
#'
#' # --- Example 2: Omitting manual keys (automatic item finding, NO reversal) ---
#' # Note: This will treat firstscale_3r and secondscale_2r as *not* reversed.
#' scored_data_auto <- scoreItemsMultiFast(
#'   scalenames = scalenames_fast,
#'   dataframe = mc_items_fast,
#'   # manual_keys = NULL, # This is the default
#'   exclude = TRUE
#' )
#'
#' print("Scores (Auto Keys, exclude = TRUE):")
#' print(scored_data_auto$scores) # Scores will differ from above due to no reversal
#' print("Negative Index (Auto Keys):")
#' print(scored_data_auto$negative_index) # Should be all FALSE
#'
#' # Example without excluding participants with too many NAs (using Auto Keys)
#' scored_data_auto_noexclude <- scoreItemsMultiFast(
#'   scalenames = scalenames_fast,
#'   dataframe = mc_items_fast,
#'   manual_keys = NULL,
#'   exclude = FALSE
#' )
#' print("Scores (Auto Keys, exclude = FALSE):")
#' print(scored_data_auto_noexclude$scores)
#'
scoreItemsMultiFast <- function(scalenames, dataframe, manual_keys = NULL, exclude = TRUE, ...) {

  # --- Input Validation ---
  if (!requireNamespace("psych", quietly = TRUE)) {
    stop("Package 'psych' needed for this function to work. Please install it.", call. = FALSE)
  }
  if (!requireNamespace("purrr", quietly = TRUE)) {
    stop("Package 'purrr' needed for this function to work. Please install it.", call. = FALSE)
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' needed for this function to work. Please install it.", call. = FALSE)
  }
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Package 'stringr' needed for this function to work. Please install it.", call. = FALSE)
  }
  if (!is.data.frame(dataframe)) {
    stop("`dataframe` must be a data frame.")
  }
  if (!is.character(scalenames) || length(scalenames) == 0) {
    stop("`scalenames` must be a character vector with at least one scale name.")
  }

  keys_list <- list()
  negative_index <- list()

  # --- Determine Keys and Negative Index ---
  if (is.null(manual_keys)) {
    # --- Scenario: Auto-detect items based on naming convention, assume NO reverse coding ---
    warning("`manual_keys` not provided. Assuming items for each scale start with the scalename followed by '.' or '_'.\nNO ITEMS WILL BE REVERSE-SCORED AUTOMATICALLY. Please ensure this is intended or provide `manual_keys`.", call. = FALSE)

    all_df_names <- names(dataframe)
    for (scale_name in scalenames) {
      # Try to find items starting with scale_name followed by . or _
      # Regex: ^scalename[._]
      item_pattern <- paste0("^", scale_name, "[._]")
      scale_items <- grep(item_pattern, all_df_names, value = TRUE)

      if (length(scale_items) == 0) {
        # Fallback: Try finding items starting *exactly* with scale_name (if scale name itself is the item?)
        # scale_items <- grep(paste0("^", scale_name, "$"), all_df_names, value = TRUE) # Less likely useful
        # if (length(scale_items) == 0) {
        stop(
          paste0("For scale '", scale_name, "', could not automatically find any items in the dataframe starting with '",
                 scale_name, ".' or '", scale_name, "_'. Please check item naming convention or provide `manual_keys`."),
          call. = FALSE
        )
        # }
      }

      # Check if found items actually exist (redundant with grep but safe)
      if (!all(scale_items %in% all_df_names)) {
        missing_in_df <- scale_items[!scale_items %in% all_df_names] # Should be empty if grep worked
        stop(paste("Internal error: Items found by pattern not in dataframe names:", paste(missing_in_df, collapse=", ")), call.=FALSE)
      }

      keys_list[[scale_name]] <- scale_items
      # Assume NO reversed items when keys are auto-detected
      negative_index[[scale_name]] <- rep(FALSE, length(scale_items))
    }

  } else {
    # --- Scenario: Manual keys provided ---
    if (!is.list(manual_keys) || is.null(names(manual_keys))) {
      stop("If provided, `manual_keys` must be a named list.")
    }
    if (!all(scalenames %in% names(manual_keys))) {
      missing_keys <- scalenames[!(scalenames %in% names(manual_keys))]
      stop(
        paste("If `manual_keys` is provided, it must contain entries for all scalenames. Missing keys for:",
              paste(missing_keys, collapse=", "))
      )
    }
    if (length(intersect(scalenames, names(manual_keys))) != length(names(manual_keys))) {
      extra_keys <- names(manual_keys)[!(names(manual_keys) %in% scalenames)]
      warning(
        paste(
          "Manual keys provided for scales not listed in scalenames:",
          paste(extra_keys, collapse=", "),
          "\nThese extra keys will be ignored."
        )
      )
    }
    # Filter manual keys to only include those in scalenames and maintain order
    manual_keys_filtered <- manual_keys[scalenames]

    # Validate items within provided keys
    purrr::walk(scalenames,
                function(scale_name) {
                  items_in_key_raw <- manual_keys_filtered[[scale_name]]
                  if (!is.character(items_in_key_raw) || length(items_in_key_raw) == 0) {
                    stop(paste0("Manual key entry for scale '", scale_name, "' must be a character vector of item names (optionally prefixed with '-')"), call. = FALSE)
                  }
                  items_in_key_clean <- stringr::str_remove(items_in_key_raw, "^-")
                  items_in_df <- names(dataframe)

                  if (!all(items_in_key_clean %in% items_in_df)) {
                    missing_items <- items_in_key_clean[!items_in_key_clean %in% items_in_df]
                    stop(
                      paste("For scale '", scale_name, "', the following items specified in manual_keys are not found in the dataframe: ",
                            paste(missing_items, collapse = ", ")), call. = FALSE
                    )
                  }
                  # Check for empty selection implicitly via the above check
                })

    keys_list <- manual_keys_filtered # Use the validated & filtered keys
    # Create negative index based *only* on manual inputs
    negative_index <- purrr::map(keys_list, function(key_vector) {
      stringr::str_detect(key_vector, "^-")
    })
  }


  # --- Select Relevant Dataframe Columns ---
  # Get unique list of all items needed, without the reversal prefix
  all_items_needed <- unique(unlist(purrr::map(keys_list, stringr::str_remove, "^-")))
  # Check if any items were identified at all
  if (length(all_items_needed) == 0) {
    stop("No items could be identified for any scale. Check scalenames, item naming, or manual_keys.", call. = FALSE)
  }
  # Check if these items exist in the dataframe (should be guaranteed by checks above, but good practice)
  items_actually_in_df <- intersect(all_items_needed, names(dataframe))
  if (length(items_actually_in_df) < length(all_items_needed)) {
    missing_items_final <- all_items_needed[!all_items_needed %in% items_actually_in_df]
    stop(paste("Internal consistency error: The following needed items are not in the dataframe:", paste(missing_items_final, collapse=", ")), call. = FALSE)
  }

  dataframe_items <- dataframe[, items_actually_in_df, drop = FALSE] # Ensure result is df


  # --- Handle Exclusions ---
  exclude_helper <- function(scale_items, n_total_items) {
    # scale_items: dataframe with only items for this scale for all participants
    # n_total_items: the total number of items belonging to this scale (from keys_list)
    if (n_total_items == 0) return(scale_items) # Avoid division by zero if scale had no items (though should error earlier)
    max_na <- n_total_items / 3
    real_na <- apply(scale_items, 1, function(x) sum(is.na(x)))
    index_na <- (real_na > max_na)
    scale_items[index_na, ] <- NA
    return(scale_items)
  }

  if (exclude) {
    dataframe_processed <- dataframe_items # Start with selected items
    for (scale_name in scalenames) {
      key_items_for_scale <- keys_list[[scale_name]]
      key_items_cleaned <- stringr::str_remove(key_items_for_scale, "^-")
      n_items <- length(key_items_for_scale)

      # Select the columns for the current scale *from the already subsetted dataframe_items*
      # Handle cases where a scale might have 0 items (should have errored before, but safer)
      if (n_items > 0 && length(key_items_cleaned) > 0 && all(key_items_cleaned %in% names(dataframe_processed))) {
        current_scale_data <- dataframe_processed[, key_items_cleaned, drop = FALSE]
        # Apply exclusion NAs based on the total number of items for this scale
        excluded_data <- exclude_helper(current_scale_data, n_items)
        # Put the potentially NA'd data back into the processed dataframe
        dataframe_processed[, key_items_cleaned] <- excluded_data
      } else if (n_items > 0) {
        # This case indicates an internal logic error if items were supposedly found but aren't in dataframe_processed
        warning(paste("Skipping exclusion for scale", scale_name, "due to item inconsistency."), call.=FALSE)
      } # If n_items is 0, do nothing.
    }
  } else {
    dataframe_processed <- dataframe_items # Use items as is if exclude is FALSE
  }


  # --- Scoring Loop using scoreFast ---
  list_scored <-
    purrr::map(scalenames, function(name) {
      keys <- keys_list[[name]] # These keys contain "-" prefix if manual_keys were used
      key_items_cleaned <- stringr::str_remove(keys, "^-")

      # Select data from the *potentially modified* dataframe_processed
      # Handle case where key_items_cleaned might be empty (should not happen if checks passed)
      if (length(key_items_cleaned) == 0 || !all(key_items_cleaned %in% names(dataframe_processed))) {
        warning(paste("Cannot score scale '", name, "' as no valid items were found or selected."), call. = FALSE)
        # Return a data frame of NAs with the correct number of rows
        out_df <- data.frame(matrix(NA_real_, nrow = nrow(dataframe_processed), ncol = 1))
        colnames(out_df) <- name
        return(out_df)
      }
      # keys need to be input as list
      keys <- list(keys)
      subdf <- dataframe_processed[, key_items_cleaned, drop = FALSE]

      # Use scoreFast - default to mean scores, no imputation
      # Allow overrides via ...
      # Pass the original 'keys' vector which includes "-" if appropriate
      score_args <- list(keys = keys, items = subdf, totals = FALSE, missing = FALSE)
      passed_args <- list(...)
      # Check for conflicts (e.g. user passing 'keys' or 'items' in ...)
      if (any(names(passed_args) %in% c("keys", "items"))) {
        warning("Arguments 'keys' or 'items' passed via ... are ignored by scoreItemsMultiFast.", call. = FALSE)
        passed_args <- passed_args[!names(passed_args) %in% c("keys", "items")]
      }
      score_args <- utils::modifyList(score_args, passed_args) # passed args override defaults

      # Catch potential errors from scoreFast (e.g., non-numeric data)
      out_scores <- tryCatch({
        do.call(psych::scoreFast, score_args)
      }, error = function(e) {
        warning(paste("Error scoring scale '", name, "': ", e$message), call. = FALSE)
        # Return NA scores on error
        matrix(NA_real_, nrow = nrow(subdf), ncol = 1)
      })


      # scoreFast returns a matrix/vector; ensure it's a matrix/df column
      if (is.vector(out_scores)) {
        out_scores <- matrix(out_scores, ncol = 1)
      }
      colnames(out_scores) <- name # Name the column appropriately
      return(as.data.frame(out_scores)) # Return as data frame column
    })

  # --- Aggregate Results ---
  # Combine the resulting single-column data frames
  scores <- dplyr::bind_cols(list_scored)

  # --- Final Output ---
  # negative_index was already created based on manual_keys or default (all FALSE)
  scaleout <- list(scores = scores, negative_index = negative_index)

  return(scaleout)
}
