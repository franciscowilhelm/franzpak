#' Correlation table for two-level data
#'
#' @param df the dataframe used
#' @param varnames the variable names as string vector
#' @param grp the variable name that identifies the level-2 group as string character
#'
#' @return a correlation table as tibble
#' @export
#'
#' @examples
cortable_multilevel <- function(df, varnames, grp) {
  statsby_summary <- psych::statsBy(df |> select( all_of(grp), all_of(varnames)), group = grp)

  icc <- statsby_summary$ICC1 |>
    round(2) # runden
  icc


  mittelwerte <- df |> summarise(across(all_of(varnames),
                                                       ~mean(.x, na.rm = TRUE),
                                                       .names = "m_{.col}")) |>
    round(2) # runden

  mittelwerte

  standardabweichung <- df |>  summarise(across(all_of(varnames),
                                                               ~sd(.x, na.rm = TRUE),
                                                               .names = "sd_{.col}")) |>
    round(2) # runden

  standardabweichung


  cortable_between <- statsby_summary$rbg |> round(2) # Zwischen Person Kor.
  cortable_within <- statsby_summary$rwg |> round(2) # Inner Person Kor.
  for(i in 1:length(cortable_between)) {
    cortable_between[i] <- str_c(cortable_between[i], vstar_assign(statsby_summary$pbg[i]))
  }

  for(i in 1:length(cortable_within)) {
    cortable_within[i] <- str_c(cortable_within[i], vstar_assign(statsby_summary$pwg[i]))
  }

  cortable <- cortable_between
  cortable[lower.tri(cortable)] <- cortable_within[lower.tri(cortable_within)] # Inner-Person Korrelationen im unteren Dreieck einfügen.
  cortable <- cortable |>   as_tibble(rownames = "var") # als data frame formatieren (später wichtig)

  cortable_integriert <- tibble(cortable["var"],
                                m = t(mittelwerte), # t() transponiert die Dimensionen der Variable, damit wir es als spalte (3x1) haben statt als zeile (1x3)
                                sd = t(standardabweichung),
                                icc = icc[2:length(icc)], # 1 überspringen da wir den ICC von "id" nicht brauchen
                                cortable[,2:ncol(cortable)])

  cortable_integriert
}
