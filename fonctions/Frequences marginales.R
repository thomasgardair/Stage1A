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
  freq <- tableau_complet %>%
    filter(across(-all_of(var), ~ . == "Total")) %>%
    select(all_of(var), nb_obs, nb_obs_pert)
  
  return(freq)
}

frequences_marginales(tableau_complet, "SEX")
