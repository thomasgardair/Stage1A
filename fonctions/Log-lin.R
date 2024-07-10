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
#' tableau_original <- generer_tableau(N)
#' tableau_perturbe <- appliquer_ckm(tableau_original, D, V)
#' tableau_perturbe <- appliquer_arrondi_aleatoire(tableau_perturbe, B)
#' liste_sous_tableaux <- recuperer_ts_sous_tableaux(
#'   tableau = tableau_perturbe,
#'   vars_cat = c("SEX","AGE","DIPL","REGION","DEPT")
#' )
#' str(liste_sous_tableaux)
#' # Exemple pour récupérer le tableau SEX * DIPL
#' liste_sous_tableaux$tabs_2Var$SEX_DIPL
#' tableau_orig <- liste_sous_tableaux$tabs_2Var$SEX_DIPL
#' tab_orig <- from_df_to_contingence(tableau_orig)
#' tab_pert <- from_df_to_contingence(tableau_orig,"nb_obs_ckm","nb_obs","nb_obs_alea")
#' RV(tab_orig,tab_pert,~ SEX + DIPL )
RV <- function(table_contingence_orig, table_contingence_pert, formula) {
  
  model_orig <- loglm1(formula, data = table_contingence_orig)
  model_pert <- loglm1(formula, data = table_contingence_pert)
  
  L2_orig <- model_orig$lrt
  L2_pert <- model_pert$lrt
  LR <- L2_pert / L2_orig
  
  return(list(L2_orig = L2_orig, L2_pert = L2_pert, LR = LR))
}

