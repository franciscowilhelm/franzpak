#' Correlation table for two-level data
#'
#' @param df the dataframe to be used, should contain all variables including grouping variable.
#' @param varnames the variable names as character vector.
#' @param grp the variable name that identifies the level-2 group as character.
#' @param varlabels character vector giving labels of the variables.
#' @param wpv Use Within-Person Variance (WPV) instead of ICC. WPV = 1-ICC.
#' @param use.001 Don't use *** to mark p < .001; this makes the table more concise.

#'
#' @return a correlation table as tibble.
#' @export
#'
#' @examples
#' cortable_multilevel(mc_twolevel, c("Y", "M", "X"), "CLUSTER")
cortable_multilevel <- function(df, varnames, grp, varlabels = NULL, wpv = FALSE, use.001 = TRUE) {
  statsby_summary <- psych::statsBy(df |> select( all_of(grp), all_of(varnames)), group = grp)


  if(wpv == FALSE) {
    icc <- statsby_summary$ICC1 |>
      papaja::print_num(gt1 = FALSE) # runden
  } else {
    icc <- statsby_summary$ICC1 |>
      (\(.x) 1 - .x)() |>
      papaja::print_num(gt1 = FALSE) # runden
  }


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


  cortable_between <- statsby_summary$rbg |> papaja::apa_num(gt1 = FALSE) # Zwischen Person Kor.
  cortable_within <- statsby_summary$rwg |> papaja::apa_num(gt1 = FALSE) # Inner Person Kor.
  for(i in 1:length(cortable_between)) {
    cortable_between[i] <- str_c(cortable_between[i], vstar_assign(statsby_summary$pbg[i], use.001))
  }

  for(i in 1:length(cortable_within)) {
    cortable_within[i] <- str_c(cortable_within[i], vstar_assign(statsby_summary$pwg[i], use.001))
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
  if(wpv == FALSE ) {
    names(cortable_integriert) <- c("Variable", "M", "SD", "ICC", str_c(1:length(varnames), "."))
  } else {
    names(cortable_integriert) <- c("Variable", "M", "SD", "WPV", str_c(1:length(varnames), "."))
    }
  cortable_integriert
}
