library(dplyr)
library(data.table)
library(rlang)


frequences_marginales <- function(tableau_complet, var){
  columns <- colnames(tableau_complet)
  columns <- columns[grepl("^[A-Z_]+$", columns)]
  columns <- setdiff(columns, var)
  
  freq <- tableau_complet %>%
    filter(if_all(all_of(columns), ~ . == "Total")) %>%
    select(all_of(var), nb_obs, nb_obs_pert)
  
  return(freq)
}
frequences_marginales(tableau_complet, "SEX")
frequences_marginales(tableau_complet, "DIPL")
frequences_marginales(tableau_complet, "AGE")
frequences_marginales(tableau_complet, "REGION")

