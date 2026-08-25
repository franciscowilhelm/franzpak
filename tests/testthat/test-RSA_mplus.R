find_rsahelpers_mplus_example <- function() {
  system.file(
    "extdata",
    "congruence_sim.out",
    package = "rsahelpers",
    mustWork = TRUE
  )
}

test_that("RSA_mplus warns and forwards to rsahelpers", {
  skip_if_not_installed("MplusAutomation")
  skip_if_not_installed("rsahelpers")

  args <- list(
    model = find_rsahelpers_mplus_example(),
    outcome = "Z",
    pred_x = "X",
    pred_y = "Y",
    pred_x2 = "XS",
    pred_xy = "XY",
    pred_y2 = "YS",
    b0 = 0,
    include_new = FALSE,
    plot = FALSE
  )

  expect_snapshot(
    old_result <- do.call(RSA_mplus, args)
  )
  new_result <- do.call(rsahelpers::RSA_mplus, args)

  expect_s3_class(old_result, "rsa_mplus")
  expect_equal(old_result$coefficients, new_result$coefficients)
  expect_equal(old_result$regression_parameters, new_result$regression_parameters)
  expect_equal(old_result$new_parameters, new_result$new_parameters)
  expect_equal(old_result$outcome, new_result$outcome)
  expect_equal(old_result$coefficient_type, new_result$coefficient_type)
})

test_that("RSA_mplus explains how to install a missing rsahelpers", {
  local_mocked_bindings(
    .rsahelpers_available = function() FALSE,
    .package = "franzpak"
  )

  expect_snapshot(
    RSA_mplus(
      model = "model.out",
      outcome = "Z",
      pred_x = "X",
      pred_y = "Y",
      pred_x2 = "XS",
      pred_xy = "XY",
      pred_y2 = "YS",
      b0 = 0,
      plot = FALSE
    ),
    error = TRUE
  )
})
