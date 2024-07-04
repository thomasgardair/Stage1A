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

