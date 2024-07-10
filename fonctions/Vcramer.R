#' Title
#'
#' @param tab tableau de contingence/matrix 
#'
#' @return V de Cramer du tableau - numeric
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
#' Vcramer(tab_orig)
Vcramer <- function(tab){
  
  # if (!is.table(tab)) {
  #   stop("L'argument doit être une table de contingence.")
  # }
  chi2 <- chisq.test(tab)$statistic
  n <- sum(tab)  
  k <- min(dim(tab)) - 1
  vcramer <- sqrt(chi2 / (n * k))
  return(as.numeric(vcramer))
}

#' Title
#'
#' @param table_orig tableau contingence avec données originales
#' @param table_pert tableau contingence avec données perturbées
#'
#' @return liste avec test khi-deux et Vcramer
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
#' Taux_Variation_Vcramer(tab_orig,tab_pert)

Taux_Variation_Vcramer <- function(table_orig, table_pert) {
  
  test_orig <- chisq.test(table_orig)
  test_pert <- chisq.test(table_pert)
  
  vcramer_original <- Vcramer(table_orig)
  vcramer_perturbe <- Vcramer(table_pert)
    
  vcramer_diff <- (abs(vcramer_original - vcramer_perturbe)/vcramer_original)*100
  
  return(list(khi_deux_orig = test_orig$statistic, khi_deux_pert = test_pert$statistic,
              p_value_orig = test_orig$p.value, p_value_pert = test_pert$p.value, 
              vcramer_orig = vcramer_original, vcramer_pert = vcramer_perturbe, vcramer_diff = vcramer_diff))
}

