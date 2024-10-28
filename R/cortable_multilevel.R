#' Correlation table for two-level data
#'
#' @param df the dataframe to be used, should contain all variables including grouping variable.
#' @param varnames the variable names as character vector.
#' @param grp the variable name that identifies the level-2 group as character.
#' @param varlabels character vector giving labels of the variables.
#'
#' @return a correlation table as tibble.
#' @export
#'
#' @examples
#' cortable_multilevel(mc_twolevel, c("Y", "M", "X"), "CLUSTER")
cortable_multilevel <- function(df, varnames, grp, varlabels = NULL) {
  statsby_summary <- psych::statsBy(df |> select( all_of(grp), all_of(varnames)), group = grp)

  icc <- statsby_summary$ICC1 |>
    papaja::print_num() # runden
  icc


  mittelwerte <- df |> summarise(across(all_of(varnames),
                                                       ~mean(.x, na.rm = TRUE),
                                                       .names = "m_{.col}")) |>
    papaja::print_num() # runden

  mittelwerte

  standardabweichung <- df |>  summarise(across(all_of(varnames),
                                                               ~sd(.x, na.rm = TRUE),
                                                               .names = "sd_{.col}")) |>
    papaja::print_num() # runden

  standardabweichung


  cortable_between <- statsby_summary$rbg |> papaja::apa_num() # Zwischen Person Kor.
  cortable_within <- statsby_summary$rwg |> papaja::apa_num() # Inner Person Kor.
  for(i in 1:length(cortable_between)) {
    cortable_between[i] <- str_c(cortable_between[i], vstar_assign(statsby_summary$pbg[i]))
  }

  for(i in 1:length(cortable_within)) {
    cortable_within[i] <- str_c(cortable_within[i], vstar_assign(statsby_summary$pwg[i]))
  }

  cortable <- cortable_between
  cortable[lower.tri(cortable)] <- cortable_within[lower.tri(cortable_within)] # Inner-Person Korrelationen im unteren Dreieck einfügen.
  diag(cortable) <- "-"
  cortable <- cortable |>   as_tibble(rownames = "var") # als data frame formatieren (später wichtig)

  cortable_integriert <- tibble(cortable["var"],
                                M = as.vector(t(mittelwerte[1,])), # t() transponiert die Dimensionen der Variable, damit wir es als spalte (3x1) haben statt als zeile (1x3)
                                SD = as.vector(t(standardabweichung[1,])),
                                ICC = icc[2:length(icc)], # 1 überspringen da wir den ICC von "id" nicht brauchen
                                cortable[,2:ncol(cortable)])

  if(!is.null(varlabels)) {
    cortable_integriert$var <- str_c(1:length(varnames), ".", varlabels)
  } else {
    cortable_integriert$var <- str_c(1:length(varnames), ".", varnames)
  }
  names(cortable_integriert) <- c("Variable", "M", "SD", "ICC", str_c(1:length(varnames), "."))
  cortable_integriert
}
