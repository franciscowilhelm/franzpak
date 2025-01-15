cortable_multilevel(mc_twolevel, c("Y", "M", "X"), "CLUSTER")
cortable_multilevel(mc_twolevel, c("Y", "M", "X"), "CLUSTER", wpv = TRUE)
cortable_multilevel(mc_twolevel, c("Y", "M", "X"), "CLUSTER", use.001 = FALSE)

load("exampleData/df_example_cli.RData")
library(dplyr)
df_example_cli <- df_example_cli |> ungroup()

cortable_integriert2 <- cortable_multilevel(df_example_cli,
                                            varnames = c("w", "y", "x"),
                                            grp = "id")
cortable_integriert2
