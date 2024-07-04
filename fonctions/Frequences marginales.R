library(dplyr)
library(data.table)
library(rlang)
freq_marg_sex <- tableau_complet %>%
  filter(DIPL == "Total" & AGE == "Total" & REGION == "Total") %>%
  select(SEX, nb_obs, nb_obs_pert)

freq_marg_sex

frequences_marginales <- function(tableau_complet, var){
  freq <- tableau_complet %>% 
    filter(across(everything(), ~ . == "Total")) %>%
    select(var, nb_obs, nb_obs_pert)
  return(freq)
}
frequences_marginales(tableau_complet, "SEX")

frequences_marginales <- function(tableau_complet, var){
  columns <- colnames(tableau_complet)
  columns <- columns[grepl("^[A-Z_]+$", columns)]
  columns <- setdiff(columns, var)
  
  freq <- tableau_complet %>%
    filter(if_all(all_of(columns), ~ . == "Total")) %>%
    select(all_of(var), nb_obs, nb_obs_pert)
  
  return(freq)
}
