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
