# Test backward compatibility - should return formatted tibble
test_that("cortable_multilevel returns formatted tibble by default", {
  result <- cortable_multilevel(mc_twolevel, c("Y", "M", "X"), "CLUSTER")
  
  expect_s3_class(result, "tbl_df")
  expect_true("Variable" %in% names(result))
  expect_true("M" %in% names(result))
  expect_true("SD" %in% names(result))
  expect_true("ICC" %in% names(result))
  
  # Check that correlations are formatted as characters (with potential significance stars)
  expect_type(result$`1.`, "character")
})

# Test WPV option
test_that("cortable_multilevel works with WPV option", {
  result <- cortable_multilevel(mc_twolevel, c("Y", "M", "X"), "CLUSTER", wpv = TRUE)
  
  expect_s3_class(result, "tbl_df")
  expect_true("WPV" %in% names(result))
  expect_false("ICC" %in% names(result))
})

# Test use.001 option
test_that("cortable_multilevel works with use.001 = FALSE", {
  result <- cortable_multilevel(mc_twolevel, c("Y", "M", "X"), "CLUSTER", use.001 = FALSE)
  
  expect_s3_class(result, "tbl_df")
  # Should still work, just different significance marking
})

# Test new list return functionality
test_that("cortable_multilevel returns list when return_list = TRUE", {
  result <- cortable_multilevel(mc_twolevel, c("Y", "M", "X"), "CLUSTER", return_list = TRUE)
  
  expect_type(result, "list")
  expect_named(result, c("formatted_table", "numeric_table", "numeric_table_p_values"))
  
  # Check formatted table
  expect_s3_class(result$formatted_table, "tbl_df")
  expect_type(result$formatted_table$`1.`, "character")  # Should be formatted
  
  # Check numeric table
  expect_s3_class(result$numeric_table, "tbl_df")
  expect_type(result$numeric_table$M, "double")  # Should be numeric
  expect_type(result$numeric_table$SD, "double")  # Should be numeric
  expect_type(result$numeric_table$ICC, "double")  # Should be numeric
  
  # Check p-values table
  expect_s3_class(result$numeric_table_p_values, "tbl_df")
  expect_type(result$numeric_table_p_values$`1.`, "double")  # Should be numeric p-values
  expect_type(result$numeric_table_p_values$`2.`, "double")  # Should be numeric p-values
  expect_type(result$numeric_table_p_values$`3.`, "double")  # Should be numeric p-values
})

# Test that all tables have same structure
test_that("formatted, numeric, and p-values tables have consistent structure", {
  result <- cortable_multilevel(mc_twolevel, c("Y", "M", "X"), "CLUSTER", return_list = TRUE)
  
  # Same number of rows and columns for all tables
  expect_equal(nrow(result$formatted_table), nrow(result$numeric_table))
  expect_equal(nrow(result$formatted_table), nrow(result$numeric_table_p_values))
  expect_equal(ncol(result$formatted_table), ncol(result$numeric_table))
  expect_equal(ncol(result$formatted_table), ncol(result$numeric_table_p_values))
  
  # Same column names for all tables
  expect_equal(names(result$formatted_table), names(result$numeric_table))
  expect_equal(names(result$formatted_table), names(result$numeric_table_p_values))
  
  # Same variable names for all tables
  expect_equal(result$formatted_table$Variable, result$numeric_table$Variable)
  expect_equal(result$formatted_table$Variable, result$numeric_table_p_values$Variable)
})

# Test with variable labels
test_that("cortable_multilevel works with variable labels", {
  result <- cortable_multilevel(mc_twolevel, c("Y", "M", "X"), "CLUSTER",
                               varlabels = c("Outcome", "Mediator", "Predictor"),
                               return_list = TRUE)
  
  expect_true(all(grepl("Outcome|Mediator|Predictor", result$formatted_table$Variable)))
  expect_true(all(grepl("Outcome|Mediator|Predictor", result$numeric_table$Variable)))
  expect_true(all(grepl("Outcome|Mediator|Predictor", result$numeric_table_p_values$Variable)))
})

# Test with example data
load("df_example.RData")
library(dplyr)

test_that("cortable_multilevel works with different datasets", {
  # Test basic functionality
  result_formatted <- cortable_multilevel(mc_twolevel, c("Y", "M", "X"), "CLUSTER")
  expect_s3_class(result_formatted, "tbl_df")
  
  # Test list return
  result_list <- cortable_multilevel(mc_twolevel, c("Y", "M", "X"), "CLUSTER", return_list = TRUE)
  expect_type(result_list, "list")
  expect_length(result_list, 3)
})

# Test p-values table specifically
test_that("p-values table contains expected values", {
  result <- cortable_multilevel(mc_twolevel, c("Y", "M", "X"), "CLUSTER", return_list = TRUE)
  
  # Check that p-values table exists and has correct structure
  expect_s3_class(result$numeric_table_p_values, "tbl_df")
  
  # Check that correlation p-values are numeric and between 0 and 1
  p_cols <- result$numeric_table_p_values[, 5:7]  # Correlation columns
  for(col in names(p_cols)) {
    p_values <- p_cols[[col]]
    # Remove NA values (diagonal)
    p_values_clean <- p_values[!is.na(p_values)]
    expect_true(all(p_values_clean >= 0 & p_values_clean <= 1))
    expect_type(p_values, "double")
  }
  
  # Check that M, SD, ICC/WPV columns are NA (no p-values for descriptives)
  expect_true(all(is.na(result$numeric_table_p_values$M)))
  expect_true(all(is.na(result$numeric_table_p_values$SD)))
  expect_true(all(is.na(result$numeric_table_p_values$ICC)))
})


# Test functionality for level-2-only variables
test_that("Level-2 variables are auto-detected and handled", {
  set.seed(42)
  mc_twolevel_l2var <- mc_twolevel |>
    dplyr::group_by(CLUSTER) |>
    dplyr::mutate(WL2 = rnorm(1)) |>
    dplyr::ungroup()

  expect_message(
    result <- cortable_multilevel(mc_twolevel_l2var, c("Y", "M", "X", "WL2"), "CLUSTER", return_list = TRUE),
    "Level-2 variable"
  )

  formatted <- result$formatted_table
  numeric <- result$numeric_table
  p_values <- result$numeric_table_p_values

  expect_equal(formatted$Variable, c("1.Y", "2.M", "3.X", "4.WL2"))
  expect_equal(numeric$Variable, formatted$Variable)
  expect_equal(p_values$Variable, formatted$Variable)

  expect_equal(formatted$ICC[4], "-")
  expect_true(is.na(numeric$ICC[4]))

  expect_equal(unlist(formatted[4, c("1.", "2.", "3.")], use.names = FALSE),
               rep("-", 3))
  expect_true(all(is.na(unlist(numeric[4, c("1.", "2.", "3.")], use.names = FALSE))))
  expect_true(all(is.na(unlist(p_values[4, c("1.", "2.", "3.")], use.names = FALSE))))

  expect_true(all(!is.na(numeric$`4.`[1:3])))
})

test_that("User-specified between variables suppress auto-detect message", {
  set.seed(42)
  mc_twolevel_l2var <- mc_twolevel |>
    dplyr::group_by(CLUSTER) |>
    dplyr::mutate(WL2 = rnorm(1)) |>
    dplyr::ungroup()

  expect_silent(
    result <- cortable_multilevel(mc_twolevel_l2var,
                                  c("Y", "M", "X", "WL2"),
                                  "CLUSTER",
                                  between = "WL2",
                                  return_list = TRUE)
  )

  formatted <- result$formatted_table
  expect_equal(formatted$Variable[4], "4.WL2")
  expect_equal(formatted$ICC[4], "-")
})
