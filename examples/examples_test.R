# scoreItemsMulti
scalenames <- c("firstscale", "secondscale")
scoreItemsMulti(scalenames, mc_items) # automatically detect reverse coded items
# manually reverse items
scoreItemsMulti(scalenames, mc_items,
                manual_keys = list(
                  secondscale = c(
                    "secondscale_1",
                    "secondscale_2",
                    "secondscale_3",
                    "secondscale_4",
                    "-secondscale_5"
                  )
                ))


# cortable_multilevel
cortable_multilevel(mc_twolevel, c("Y", "M", "X"), "CLUSTER")
cortable_multilevel(mc_twolevel, c("Y", "M", "X"), "CLUSTER", return_list = TRUE)

# mplus coef tools
coef_wrapper(mplusmodel, params = c('regression', 'new'), addci = TRUE)

# pivot_longer_multilevel

# generate wide dataset with 10 clusters, x measured three times and y measured three times
df_twolevel_wide <- tibble(id = 1:10, x_t1 = rep(1:5,2), x_t2 = rep(1:5,2), x_t3 = rep(1:5,2),
                           y_t1 = rep(1:5,2), y_t2 = rep(1:5,2), y_t3 = rep(1:5,2))
# pivot longer
pivot_longer_multilevel(df_twolevel_wide, c("x", "y"), 3)

# with NAs due to some level-2 not having data on all level-1 measurements.
# generate wide dataset with 10 clusters, x measured three times and y measured three times
df_twolevel_wide <- tibble(id = 1:10, x_t1 = rep(1:5,2), x_t2 = rep(1:5,2), x_t3 = rep(1:5,2),
                           y_t1 = rep(1:5,2), y_t2 = rep(1:5,2), y_t3 = rep(1:5,2))
df_twolevel_wide[1,"x_t1"] <- NA
df_twolevel_wide[1,"y_t1"] <- NA
# pivot longer
pivot_longer_multilevel(df_twolevel_wide, c("x", "y"), 3)
