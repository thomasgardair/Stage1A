#' Title
#'
#' @param tableau_complet  tableau de données complet sous forme de dtaframe/datatable
#' @param nb_obs nom colonne obs originales
#' @param nb_obs_pert nom colonne obs perturbées
#' @param paired = TRUE
#'
#' @return test de wilcoxon entre données originales et perturbés
#' @export
#'
#' @examples
#' library(dplyr)
#' tableau_complet <- generer_tableau(100)
#' tab_avec_AL <- appliquer_arrondi_aleatoire(tableau_complet, 10)
#' wilcoxon_test(tab_avec_AL, "nb_obs", "nb_obs_alea", paired = TRUE)
wilcoxon_test <- function(tableau_complet, nb_obs, nb_obs_pert, paired = TRUE) {
  
  
  obs <- tableau_complet[[nb_obs]]
  obs_pert <- tableau_complet[[nb_obs_pert]]
  
  test <- wilcox.test(obs, obs_pert, paired = paired)
  p_value <-test$p.value
  
  return(list(
    p_value = p_value
  ))
}


