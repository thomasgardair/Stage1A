rm(list = ls())

tab_sans_marges <- tableau_complet %>% filter(REGION != "Total" & AGE != "Total" & SEX != "Total" & DIPL != "Total")

columns <- colnames(tab_sans_marges)
columns <- columns[grepl("^[A-Z_]+$", columns)]

combinaisons <- combn(columns_upper, 2, simplify = FALSE)

for (vars in combinaisons) {
  formula_obs <- as.formula(paste("nb_obs ~", paste(vars, collapse = " + ")))
  formula_obs_pert <- as.formula(paste("nb_obs_pert ~", paste(vars, collapse = " + ")))
  
  table_obs <- xtabs(formula_obs, data = tab_sans_marges)
  table_obs_pert <- xtabs(formula_obs_pert, data = tab_sans_marges)
  
  
  table_orig <- paste(paste(vars, collapse = "_"), "orig",sep="_")
  table_pert <- paste(paste(vars, collapse = "_"), "pert",sep="_")
  
  assign(table_orig, table_obs, envir = .GlobalEnv)
  assign(table_pert, table_obs_pert, envir = .GlobalEnv)
}
#Calcul tout les sous tableaux possible a partir du tableau complet 

sous_tableau <- function(tableau_complet) {
  tab_sans_marges <- tableau_complet %>% 
    filter(across(everything(), ~ . != "Total"))
  
  columns <- colnames(tab_sans_marges)
  columns <- columns[grepl("^[A-Z_]+$", columns)]
  
  combinations <- combn(columns, 2, simplify = FALSE)
  
  for (vars in combinations) {
    formula_obs <- as.formula(paste("nb_obs ~", paste(vars, collapse = " + ")))
    formula_obs_pert <- as.formula(paste("nb_obs_pert ~", paste(vars, collapse = " + ")))
    
    table_obs <- xtabs(formula_obs, data = tab_sans_marges)
    table_obs_pert <- xtabs(formula_obs_pert, data = tab_sans_marges)
    
    table_orig <- paste(paste(vars, collapse = "_"), "orig", sep = "_")
    table_pert <- paste(paste(vars, collapse = "_"), "pert", sep = "_")
    
    assign(table_orig, table_obs, envir = .GlobalEnv)
    assign(table_pert, table_obs_pert, envir = .GlobalEnv)
  
  }
}

#Calcul les distances pour tout les tableaux de contingences 

calcul_distance <- function(table_orig, table_pert) {
  obs <- as.vector(table_orig)
  obs_pert <- as.vector(table_pert)
  
  AAD <- mean(abs(obs - obs_pert))
  
  HD <- mean(sqrt((1/2) * (sqrt(obs) - sqrt(obs_pert))^2))
  
  RAD <- mean(abs(obs - obs_pert) / obs)
  
  return(list(AAD = AAD, HD = HD, RAD = RAD))
}

