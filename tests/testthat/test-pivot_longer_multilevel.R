test_that("first three entries of x are as expected", {
  df_twolevel_wide <- tibble(id = 1:10, x_t1 = rep(1:5,2), x_t2 = rep(1:5,2), x_t3 = rep(1:5,2),
                             y_t1 = rep(1:5,2), y_t2 = rep(1:5,2), y_t3 = rep(1:5,2))
  df_twolevel_wide[1,"x_t1"] <- NA
  df_twolevel_wide[1,"y_t1"] <- NA
  # pivot longer
  out <- pivot_longer_multilevel(df_twolevel_wide, c("x", "y"), 3)
  expect_equal(out[1:3,"x"], tibble(x = c(1,1,2)))
})
