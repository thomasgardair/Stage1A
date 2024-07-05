# Modélisation log-linéaire
install.packages("MASS")
library(MASS)

tab_sans_marges <- tableau_complet %>% filter(REGION != "Total" & AGE != "Total" & SEX != "Total" & DIPL != "Total")

table_contingence_orig  <- xtabs(nb_obs ~ REGION + AGE, data = tab_sans_marges)
table_contingence_pert  <- xtabs(nb_obs_pert ~ REGION + AGE, data = tab_sans_marges)


RV <- function(table_contingence_orig, table_contingence_pert, formula) {
  model_orig <- loglm(formula, data = table_contingence_orig)
  model_pert <- loglm(formula, data = table_contingence_pert)
  
  L2_orig <- model_orig$lrt
  L2_pert <- model_pert$lrt
  LR <- L2_pert / L2_orig
  
  return(list(L2_orig = L2_orig, L2_pert = L2_pert, LR = LR))
}

formula <- ~ AGE + DEPT
