data <- "X:/HAB-INVEST-CONFIDENTIALITE/QPV/Pole_Emploi/liste_tableaux_pole_emploi_avant_pert.rds" 

liste_tableaux <-readRDS(data)

str(liste_tableaux)

tableau_1 <-liste_tableaux[[1]]
tableau_1 <- tableau_1 %>% select(-rkeys_max,-ck)

