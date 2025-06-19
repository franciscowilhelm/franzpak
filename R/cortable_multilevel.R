#' Correlation table for two-level data
#'
#' @param df the dataframe to be used, should contain all variables including grouping variable.
#' @param varnames the variable names as character vector.
#' @param grp the variable name that identifies the level-2 group as character.
#' @param varlabels character vector giving labels of the variables.
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
#' # Return both formatted and numeric tables
#' result <- cortable_multilevel(mc_twolevel, c("Y", "M", "X"), "CLUSTER", return_list = TRUE)
#' result$formatted_table        # APA-formatted table
#' result$numeric_table          # Numeric table for further analysis
#' result$numeric_table_p_values # P-values for significance testing
cortable_multilevel <- function(df, varnames, grp, varlabels = NULL, wpv = FALSE, use.001 = TRUE, return_list = FALSE) {
  
  # Calculate multilevel statistics using psych::statsBy
  # This provides between-group and within-group correlations, ICCs, and p-values
  statsby_summary <- psych::statsBy(df |> select(all_of(grp), all_of(varnames)), group = grp)

  # Calculate ICC or WPV (Within-Person Variance) - keep numeric for later formatting
  if(wpv == FALSE) {
    icc_numeric <- statsby_summary$ICC1  # Keep original numeric values
  } else {
    icc_numeric <- 1 - statsby_summary$ICC1  # Calculate WPV as 1-ICC, keep numeric
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

  # Extract correlation matrices - keep numeric versions
  cortable_between_numeric <- statsby_summary$rbg  # Between-person correlations (numeric)
  cortable_within_numeric <- statsby_summary$rwg   # Within-person correlations (numeric)
  
  # Extract p-values for significance testing
  p_between <- statsby_summary$pbg  # p-values for between-person correlations
  p_within <- statsby_summary$pwg   # p-values for within-person correlations

  # Create numeric correlation table (upper triangle: between-person, lower triangle: within-person)
  cortable_numeric <- cortable_between_numeric
  cortable_numeric[lower.tri(cortable_numeric)] <- cortable_within_numeric[lower.tri(cortable_within_numeric)]
  diag(cortable_numeric) <- NA  # Set diagonal to NA for numeric version
  cortable_numeric_df <- cortable_numeric |> as_tibble(rownames = "var")

  # Create numeric integrated table
  cortable_integriert_numeric <- tibble(
    var = if(!is.null(varlabels)) str_c(1:length(varnames), ".", varlabels) else str_c(1:length(varnames), ".", varnames),
    M = as.vector(t(mittelwerte_numeric[1,])),  # Transpose to get column vector
    SD = as.vector(t(standardabweichung_numeric[1,])),
    ICC_WPV = icc_numeric[2:length(icc_numeric)],  # Skip first element (group variable ICC)
    cortable_numeric_df[,2:ncol(cortable_numeric_df)]  # Add correlation columns
  )
  
  # Set appropriate column names for numeric table
  if(wpv == FALSE) {
    names(cortable_integriert_numeric) <- c("Variable", "M", "SD", "ICC", str_c(1:length(varnames), "."))
  } else {
    names(cortable_integriert_numeric) <- c("Variable", "M", "SD", "WPV", str_c(1:length(varnames), "."))
  }

  # Create p-values table (upper triangle: between-person p-values, lower triangle: within-person p-values)
  cortable_p_values <- p_between
  cortable_p_values[lower.tri(cortable_p_values)] <- p_within[lower.tri(p_within)]
  diag(cortable_p_values) <- NA  # Set diagonal to NA for p-values version
  cortable_p_values_df <- cortable_p_values |> as_tibble(rownames = "var")

  # Create p-values integrated table
  cortable_integriert_p_values <- tibble(
    var = if(!is.null(varlabels)) str_c(1:length(varnames), ".", varlabels) else str_c(1:length(varnames), ".", varnames),
    M = NA_real_,  # No p-values for descriptive statistics
    SD = NA_real_, # No p-values for descriptive statistics
    ICC_WPV = NA_real_,  # No p-values for ICC/WPV in this context
    cortable_p_values_df[,2:ncol(cortable_p_values_df)]  # Add p-value columns
  )
  
  # Set appropriate column names for p-values table
  if(wpv == FALSE) {
    names(cortable_integriert_p_values) <- c("Variable", "M", "SD", "ICC", str_c(1:length(varnames), "."))
  } else {
    names(cortable_integriert_p_values) <- c("Variable", "M", "SD", "WPV", str_c(1:length(varnames), "."))
  }

  # Now create formatted versions for display
  # Format ICC/WPV values
  if(wpv == FALSE) {
    icc_formatted <- icc_numeric |> papaja::print_num(gt1 = FALSE)
  } else {
    icc_formatted <- icc_numeric |> papaja::print_num(gt1 = FALSE)
  }

  # Format descriptive statistics
  mittelwerte_formatted <- mittelwerte_numeric |> papaja::print_num()
  standardabweichung_formatted <- standardabweichung_numeric |> papaja::print_num()

  # Format correlations and add significance stars
  cortable_between_formatted <- statsby_summary$rbg |> papaja::apa_num(gt1 = FALSE)
  cortable_within_formatted <- statsby_summary$rwg |> papaja::apa_num(gt1 = FALSE)
  
  # Add significance stars to formatted correlations
  for(i in 1:length(cortable_between_formatted)) {
    cortable_between_formatted[i] <- str_c(cortable_between_formatted[i], vstar_assign(p_between[i], use.001))
  }

  for(i in 1:length(cortable_within_formatted)) {
    cortable_within_formatted[i] <- str_c(cortable_within_formatted[i], vstar_assign(p_within[i], use.001))
  }

  # Create formatted correlation table
  cortable_formatted <- cortable_between_formatted
  cortable_formatted[lower.tri(cortable_formatted)] <- cortable_within_formatted[lower.tri(cortable_within_formatted)]
  diag(cortable_formatted) <- "-"  # Set diagonal to dash for formatted version
  cortable_formatted_df <- cortable_formatted |> as_tibble(rownames = "var")

  # Create formatted integrated table
  cortable_integriert_formatted <- tibble(
    var = if(!is.null(varlabels)) str_c(1:length(varnames), ".", varlabels) else str_c(1:length(varnames), ".", varnames),
    M = as.vector(t(mittelwerte_formatted[1,])),
    SD = as.vector(t(standardabweichung_formatted[1,])),
    ICC_WPV = icc_formatted[2:length(icc_formatted)],
    cortable_formatted_df[,2:ncol(cortable_formatted_df)]
  )

  # Set appropriate column names for formatted table
  if(wpv == FALSE) {
    names(cortable_integriert_formatted) <- c("Variable", "M", "SD", "ICC", str_c(1:length(varnames), "."))
  } else {
    names(cortable_integriert_formatted) <- c("Variable", "M", "SD", "WPV", str_c(1:length(varnames), "."))
  }

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
