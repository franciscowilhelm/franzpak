find_rsa_mplus_example <- function() {
  installed_path <- system.file("extdata", "congruence_sim.out", package = "franzpak")
  if (nzchar(installed_path)) {
    return(installed_path)
  }

  normalizePath(file.path("inst", "extdata", "congruence_sim.out"), mustWork = TRUE)
}

test_that("RSA_mplus extracts unstandardized coefficients from Mplus output", {
  skip_if_not_installed("MplusAutomation")

  model_path <- find_rsa_mplus_example()

  expect_warning(
    result <- RSA_mplus(
      model = model_path,
      outcome = "Z",
      pred_x = "X",
      pred_y = "Y",
      pred_x2 = "XS",
      pred_xy = "XY",
      pred_y2 = "YS",
      new_labels = c("CS", "CC", "IS", "IC", "A5"),
      plot = FALSE
    ),
    "using `b0 = 0`"
  )

  expect_s3_class(result, "rsa_mplus")
  expect_equal(
    unname(result$coefficients[c("x", "y", "x2", "xy", "y2", "b0")]),
    c(0.326, 0.166, -0.058, 0.069, -0.085, 0),
    tolerance = 1e-8
  )
  expect_equal(result$regression_parameters$term, c("x", "y", "x2", "xy", "y2"))
  expect_equal(result$new_parameters$Label, c("CS", "CC", "IS", "IC", "A5"))
  expect_null(result$plot)
})

test_that("RSA_mplus accepts an mplus.model object and standardized coefficients", {
  skip_if_not_installed("MplusAutomation")

  model_path <- find_rsa_mplus_example()
  mplus_model <- MplusAutomation::readModels(model_path, quiet = TRUE)

  expect_warning(
    result <- RSA_mplus(
      model = mplus_model,
      outcome = "Z",
      pred_x = "X",
      pred_y = "Y",
      pred_x2 = "XS",
      pred_xy = "XY",
      pred_y2 = "YS",
      coef_type = "stdyx",
      include_new = FALSE,
      plot = FALSE
    ),
    "using `b0 = 0`"
  )

  expect_equal(
    unname(result$coefficients[c("x", "y", "x2", "xy", "y2")]),
    c(0.432, 0.200, -0.080, 0.086, -0.097),
    tolerance = 1e-8
  )
  expect_null(result$new_parameters)
})
