Vcramer <- function(tab) {
  if (!is.table(tab)) {
    stop("L'argument doit être une table de contingence.")
  }
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
#' Taux_Variation_Vcramer(liste_sous_tableaux_orig$tabs_2Vars$SEX_DIPL,liste_sous_tableaux_pert$tabs_2Var$SEX_DIPL)
#' 
Taux_Variation_Vcramer <- function(table_orig, table_pert) {
  

  test_orig <- chisq.test(table_orig)
  test_pert <- chisq.test(table_pert)
  
  vcramer_diff <- "L'indépendance n'est pas rejetée pour l'un ou les deux tableaux."
  
  if (test_orig$p.value < 0.05 && test_pert$p.value < 0.05) {
    vcramer_original <- Vcramer(table_pert)
    vcramer_perturbe <- Vcramer(table_pert)
    
    vcramer_diff <- (abs(vcramer_original - vcramer_perturbed)/vcramer_original)*100
  }
  
  return(list(test_orig = test_orig, test_pert = test_pert, vcramer_original = vcramer_original, vcramer_perturbe=vcramer_perturbe, vcramer_diff = vcramer_diff))
}

