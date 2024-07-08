# Modélisation log-linéaire
install.packages("MASS")
library(MASS)

#' Title
#'
#' @param table_contingence_orig 
#' @param table_contingence_pert 
#' @param formula 
#'
#' @return
#' @export
#'
#' @examples
#' library(dplyr)
#' tableau_complet <- generer_tableau(100)
#' tab_avec_AL <- appliquer_arrondi_aleatoire(tableau_complet, 10)
#' VR(tab_avec_AL, "nb_obs", "nb_obs_alea")
#' 
RV <- function(table_contingence_orig, table_contingence_pert, formula) {
  
  model_orig <- loglm(formula, data = table_contingence_orig)
  model_pert <- loglm(formula, data = table_contingence_pert)
  
  L2_orig <- model_orig$lrt
  L2_pert <- model_pert$lrt
  LR <- L2_pert / L2_orig
  
  return(list(L2_orig = L2_orig, L2_pert = L2_pert, LR = LR))
}

formula <- ~ AGE + DEPT
