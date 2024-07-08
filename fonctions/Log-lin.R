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
#' liste_sous_tableaux_orig <- recuperer_ts_sous_tableaux(
#'   tableau = tab_avec_AL,
#'   vars_cat = c("SEX","AGE","DIPL","REGION")
#' )
#' liste_sous_tableau_pert <- recuperer_ts_sous_tableaux(
#' tableau = tab_avec_AL,
#' vars_cat = c("SEX","AGE","DIPL","REGION"),
#' vars_num = "nb_obs_alea", mod_total = "Total"
#' )
#' 
#' RV(liste_sous_tableaux_orig$tabs_2Vars$SEX_DIPL,liste_sous_tableaux_pert$tabs_2Var$SEX_DIPL,~ SEX + DIPL )
RV <- function(table_contingence_orig, table_contingence_pert, formula) {
  
  model_orig <- loglm(formula, data = table_contingence_orig)
  model_pert <- loglm(formula, data = table_contingence_pert)
  
  L2_orig <- model_orig$lrt
  L2_pert <- model_pert$lrt
  LR <- L2_pert / L2_orig
  
  return(list(L2_orig = L2_orig, L2_pert = L2_pert, LR = LR))
}

