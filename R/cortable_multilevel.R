#' Extract variable labels from dataframe
#'
#' Helper function to extract variable labels from dataframe columns.
#' Labels can be set by haven or sjlabelled packages and are stored as
#' the "label" attribute on each column.
#'
#' @param df Dataframe containing variables
#' @param varnames Character vector of variable names to extract labels for
#'
#' @return Character vector of labels (or variable names if no label found)
#' @noRd
extract_var_labels <- function(df, varnames) {
  sapply(varnames, function(var) {
    if (var %in% names(df)) {
      label <- attr(df[[var]], "label", exact = TRUE)
      if (!is.null(label) && is.character(label) && length(label) == 1 && nzchar(label)) {
        return(label)
      }
    }
    return(var)  # Fall back to variable name if no label found
  }, USE.NAMES = FALSE)
}

#' Correlation table for two-level data
#'
#' @param df the dataframe to be used, should contain all variables including grouping variable.
#' @param varnames the variable names as character vector.
#' @param grp the variable name that identifies the level-2 group as character.
#' @param varlabels character vector giving labels of the variables. If NULL (default),
#'   the function will attempt to auto-detect labels from the dataframe using the "label"
#'   attribute (set by haven or sjlabelled packages). If no labels are found, variable
#'   names will be used.
#' @param between character vector of variables that vary only at Level 2.
#' @param wpv Use Within-Person Variance (WPV) instead of ICC. WPV = 1-ICC.
#' @param use.001 Don't use *** to mark p < .001; this makes the table more concise.
#' @param return_list Logical. If TRUE, returns a list with both formatted and numeric tables.
#'   If FALSE, returns only the formatted table (default behavior for backward compatibility).
#'
#' @return If return_list = FALSE: a formatted correlation table as tibble.
#'   If return_list = TRUE: a list containing:
#'   \itemize{
#'     \item formatted_table: The formatted correlation table with APA-style formatting
#'     \item numeric_table: The correlation table with numeric values retained
#'     \item numeric_table_p_values: The correlation table with p-values for significance testing
#'   }
#' @export
#'
#' @examples
#' # Return formatted table only (backward compatible)
#' cortable_multilevel(mc_twolevel, c("Y", "M", "X"), "CLUSTER")
#'
#' # Auto-detect variable labels from haven/sjlabelled attributes
#' # If your dataframe has label attributes (e.g., from haven::read_spss()),
#' # they will be automatically detected and used in the table
#' cortable_multilevel(mc_twolevel, c("Y", "M", "X"), "CLUSTER")
#'
#' # Manual labels override auto-detection
#' cortable_multilevel(mc_twolevel, c("Y", "M", "X"), "CLUSTER",
#'                     varlabels = c("Outcome", "Mediator", "Predictor"))
#'
#' # Return both formatted and numeric tables
#' result <- cortable_multilevel(mc_twolevel, c("Y", "M", "X"), "CLUSTER", return_list = TRUE)
#' result$formatted_table        # APA-formatted table
#' result$numeric_table          # Numeric table for further analysis
#' result$numeric_table_p_values # P-values for significance testing
cortable_multilevel <- function(df, varnames, grp, varlabels = NULL, between = NULL, wpv = FALSE, use.001 = TRUE, return_list = FALSE) {
  
  # Calculate multilevel statistics using psych::statsBy
  # This provides between-group and within-group correlations, ICCs, and p-values
  statsby_summary <- psych::statsBy(df |>
                                      select(all_of(grp), all_of(varnames)),
                                    group = grp)

  # Helper to align statsBy matrices that use ".bg"/".wg" suffixes
  strip_level_suffix <- function(x) {
    sub("\\.(bg|wg)$", "", x)
  }

  reorder_stats_matrix <- function(mat, target_names) {
    if(is.null(mat)) {
      stop("Expected correlation matrix from psych::statsBy(), but received NULL.")
    }
    if(!is.null(rownames(mat))) {
      rownames(mat) <- strip_level_suffix(rownames(mat))
    }
    if(!is.null(colnames(mat))) {
      colnames(mat) <- strip_level_suffix(colnames(mat))
    }
    missing_rows <- setdiff(target_names, rownames(mat))
    missing_cols <- setdiff(target_names, colnames(mat))
    if(length(missing_rows) > 0 || length(missing_cols) > 0) {
      stop("Mismatch between `varnames` and correlation matrices returned by psych::statsBy(). Missing: ",
           stringr::str_c(unique(c(missing_rows, missing_cols)), collapse = ", "))
    }
    mat[target_names, target_names, drop = FALSE]
  }

  # Prepare ICC values (exclude the grouping variable)
  icc_raw <- statsby_summary$ICC1
  if(length(icc_raw) != (length(varnames) + 1)) {
    stop("Unexpected ICC vector length returned by psych::statsBy().")
  }
  names(icc_raw) <- c(grp, varnames)
  icc_by_var <- icc_raw[varnames]

  # Handle user-specified Level-2 variables
  user_between <- character(0)
  if(!is.null(between)) {
    missing_between <- setdiff(between, varnames)
    if(length(missing_between) > 0) {
      stop("Variables provided via `between` must also appear in `varnames`: ",
           stringr::str_c(missing_between, collapse = ", "))
    }
    user_between <- varnames[varnames %in% between]
  }

  # Automatically detect Level-2 variables based on ICC ~= 1
  tolerance_icc <- 1e-6
  auto_between <- varnames[which(!is.na(icc_by_var) & (icc_by_var >= (1 - tolerance_icc)))]
  detected_only_auto <- setdiff(auto_between, user_between)
  if(length(detected_only_auto) > 0) {
    for(var in detected_only_auto) {
      message("Variable '", var,
              "' has been detected as a Level-2 variable with variance only at the between-level. Check if this is intended.")
    }
  }

  between_vars <- unique(c(user_between, auto_between))
  between_vars <- varnames[varnames %in% between_vars]
  within_vars <- setdiff(varnames, between_vars)
  varnames_ordered <- c(within_vars, between_vars)

  # Handle variable labels: manual override, auto-detection, or fallback to variable names
  if(!is.null(varlabels)) {
    # User provided manual labels - use those
    if(length(varlabels) != length(varnames)) {
      stop("Length of `varlabels` must match length of `varnames`.")
    }
    varlabels_ordered <- varlabels[match(varnames_ordered, varnames)]
  } else {
    # Auto-detect labels from dataframe attributes (haven/sjlabelled)
    auto_labels <- extract_var_labels(df, varnames)
    varlabels_ordered <- auto_labels[match(varnames_ordered, varnames)]
  }
  display_labels <- varlabels_ordered

  # Calculate ICC or WPV (Within-Person Variance) - keep numeric for later formatting
  if(wpv == FALSE) {
    icc_numeric <- icc_by_var
  } else {
    icc_numeric <- 1 - icc_by_var
  }
  icc_numeric <- icc_numeric[varnames_ordered]
  between_idx <- match(between_vars, varnames_ordered)
  if(length(between_idx) > 0) {
    icc_numeric[between_idx] <- NA_real_
  }

  # Calculate descriptive statistics - keep numeric versions before formatting
  mittelwerte_numeric <- df |>
    summarise(across(all_of(varnames),
                     ~mean(.x, na.rm = TRUE),
                     .names = "m_{.col}"))

  standardabweichung_numeric <- df |>
    summarise(across(all_of(varnames),
                     ~sd(.x, na.rm = TRUE),
                     .names = "sd_{.col}"))

  mean_numeric_vec <- mittelwerte_numeric |>
    select(all_of(stringr::str_c("m_", varnames_ordered))) |>
    as.matrix() |> as.vector()

  sd_numeric_vec <- standardabweichung_numeric |>
    select(all_of(stringr::str_c("sd_", varnames_ordered))) |>
    as.matrix() |> as.vector()

  # Extract correlation matrices - keep numeric versions
  cortable_between_numeric <- reorder_stats_matrix(statsby_summary$rbg, varnames_ordered)
  cortable_within_numeric <- reorder_stats_matrix(statsby_summary$rwg, varnames_ordered)
  
  # Extract p-values for significance testing
  p_between <- reorder_stats_matrix(statsby_summary$pbg, varnames_ordered)
  p_within <- reorder_stats_matrix(statsby_summary$pwg, varnames_ordered)

  # Create numeric correlation table (upper triangle: between-person, lower triangle: within-person)
  cortable_numeric <- cortable_between_numeric
  cortable_numeric[lower.tri(cortable_numeric)] <- cortable_within_numeric[lower.tri(cortable_within_numeric)]
  if(length(between_idx) > 0) {
    lower_mask <- lower.tri(cortable_numeric) &
      (row(cortable_numeric) %in% between_idx | col(cortable_numeric) %in% between_idx)
    cortable_numeric[lower_mask] <- NA_real_
  }
  diag(cortable_numeric) <- NA
  cortable_numeric_df <- cortable_numeric |> as_tibble(rownames = "var")
  cor_numeric_only <- cortable_numeric_df |> select(-var)
  names(cor_numeric_only) <- stringr::str_c("cor_", seq_along(varnames_ordered))

  # Create numeric integrated table
  cortable_integriert_numeric <- tibble(
    Variable = stringr::str_c(seq_along(varnames_ordered), ".", display_labels),
    M = mean_numeric_vec,
    SD = sd_numeric_vec,
    ICC_WPV = icc_numeric
  ) |>
    bind_cols(cor_numeric_only)

  # Create p-values table (upper triangle: between-person p-values, lower triangle: within-person p-values)
  cortable_p_values <- p_between
  cortable_p_values[lower.tri(cortable_p_values)] <- p_within[lower.tri(p_within)]
  if(length(between_idx) > 0) {
    lower_mask_p <- lower.tri(cortable_p_values) &
      (row(cortable_p_values) %in% between_idx | col(cortable_p_values) %in% between_idx)
    cortable_p_values[lower_mask_p] <- NA_real_
  }
  diag(cortable_p_values) <- NA
  cortable_p_values_df <- cortable_p_values |> as_tibble(rownames = "var")
  cor_p_values_only <- cortable_p_values_df |> select(-var)
  names(cor_p_values_only) <- stringr::str_c("cor_", seq_along(varnames_ordered))

  # Create p-values integrated table
  cortable_integriert_p_values <- tibble(
    Variable = stringr::str_c(seq_along(varnames_ordered), ".", display_labels),
    M = NA_real_,
    SD = NA_real_,
    ICC_WPV = NA_real_
  ) |>
    bind_cols(cor_p_values_only)

  # Now create formatted versions for display
  # Format ICC/WPV values
  icc_formatted <- icc_numeric |> papaja::print_num(gt1 = FALSE) |> unlist() |> unname()
  if(length(between_idx) > 0) {
    icc_formatted[between_idx] <- "-"
  }

  # Format descriptive statistics
  mittelwerte_formatted <- mittelwerte_numeric |> papaja::print_num()
  standardabweichung_formatted <- standardabweichung_numeric |> papaja::print_num()

  mean_formatted_vec <- mittelwerte_formatted |>
    select(all_of(stringr::str_c("m_", varnames_ordered))) |>
    as.matrix() |> as.vector()

  sd_formatted_vec <- standardabweichung_formatted |>
    select(all_of(stringr::str_c("sd_", varnames_ordered))) |>
    as.matrix() |> as.vector()

  # Format correlations and add significance stars
  cortable_between_formatted <- statsby_summary$rbg |> papaja::apa_num(gt1 = FALSE)
  cortable_between_formatted <- reorder_stats_matrix(cortable_between_formatted, varnames_ordered)
  cortable_within_formatted <- statsby_summary$rwg |> papaja::apa_num(gt1 = FALSE)
  cortable_within_formatted <- reorder_stats_matrix(cortable_within_formatted, varnames_ordered)
  
  # Add significance stars to formatted correlations
  for(i in seq_len(length(cortable_between_formatted))) {
    cortable_between_formatted[i] <- stringr::str_c(cortable_between_formatted[i], vstar_assign(p_between[i], use.001))
  }

  for(i in seq_len(length(cortable_within_formatted))) {
    cortable_within_formatted[i] <- stringr::str_c(cortable_within_formatted[i], vstar_assign(p_within[i], use.001))
  }

  # Create formatted correlation table
  cortable_formatted <- cortable_between_formatted
  cortable_formatted[lower.tri(cortable_formatted)] <- cortable_within_formatted[lower.tri(cortable_within_formatted)]
  if(length(between_idx) > 0) {
    lower_mask_formatted <- lower.tri(cortable_formatted) &
      (row(cortable_formatted) %in% between_idx | col(cortable_formatted) %in% between_idx)
    cortable_formatted[lower_mask_formatted] <- "-"
  }
  diag(cortable_formatted) <- "-"
  cortable_formatted_df <- cortable_formatted |> as_tibble(rownames = "var")
  cor_formatted_only <- cortable_formatted_df |> select(-var)
  names(cor_formatted_only) <- stringr::str_c("cor_", seq_along(varnames_ordered))

  # Create formatted integrated table
  cortable_integriert_formatted <- tibble(
    Variable = stringr::str_c(seq_along(varnames_ordered), ".", display_labels),
    M = mean_formatted_vec,
    SD = sd_formatted_vec,
    ICC_WPV = icc_formatted
  ) |>
    bind_cols(cor_formatted_only)

  # Set appropriate column names
  icc_col_name <- ifelse(wpv, "WPV", "ICC")
  correlation_colnames <- stringr::str_c(seq_along(varnames_ordered), ".")

  names(cortable_integriert_numeric) <- c("Variable", "M", "SD", icc_col_name, correlation_colnames)
  names(cortable_integriert_formatted) <- c("Variable", "M", "SD", icc_col_name, correlation_colnames)
  names(cortable_integriert_p_values) <- c("Variable", "M", "SD", icc_col_name, correlation_colnames)

  # Return based on return_list parameter
  if(return_list) {
    return(list(
      formatted_table = cortable_integriert_formatted,
      numeric_table = cortable_integriert_numeric,
      numeric_table_p_values = cortable_integriert_p_values
    ))
  } else {
    # Backward compatibility: return only formatted table
    return(cortable_integriert_formatted)
  }
}
