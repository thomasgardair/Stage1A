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
#'   vars_cat = c("SEX","AGE","DIPL","REGION","DEPT")
#' )
#' liste_sous_tableaux_alea <- recuperer_ts_sous_tableaux(
#' tableau = tab_avec_AL,
#' vars_cat = c("SEX","AGE","DIPL","REGION","DEPT"),
#' vars_num = "nb_obs_alea", mod_total = "Total"
#' )
#' tableau_orig <- liste_sous_tableaux_orig$tabs_2Var$SEX_DIPL
#' tableau_pert <- liste_sous_tableaux_alea$tabs_2Var$SEX_DIPL
#' tab_orig <- from_df_to_contingence(tableau_orig)
#' tab_pert <- from_df_to_contingence(tableau_pert,"nb_obs_alea")
#' RV(tab_orig,tab_pert,~ SEX + DIPL )
RV <- function(table_contingence_orig, table_contingence_pert, formula) {
  
  model_orig <- loglm(formula, data = table_contingence_orig)
  model_pert <- loglm(formula, data = table_contingence_pert)
  
  L2_orig <- model_orig$lrt
  L2_pert <- model_pert$lrt
  LR <- L2_pert / L2_orig
  
  return(list(L2_orig = L2_orig, L2_pert = L2_pert, LR = LR))
}

