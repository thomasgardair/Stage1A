library(dplyr)
library(data.table)

freq_marg_sex <- tableau_complet %>%
  filter(DIPL == "Total" & AGE == "Total" & REGION == "Total") %>%
  select(SEX, nb_obs)

freq_marg_sex
