#Fonction pour calculer les tests entre les valeurs originales et perturbées

#' Title
#'
#' @param tableau_complet tableau de données complet sous forme de dtaframe/datatable
#' @param nb_obs nom colonne obs originales
#' @param nb_obs_pert nom colonne obs perturbées
#'
#' @return test de spearman entre données originales et perturbés
#' @export
#'
#' @examples
#' library(dplyr)
#' tableau_complet <- generer_tableau(100)
#' tab_avec_AL <- appliquer_arrondi_aleatoire(tableau_complet, 10)
#' spearman_test(tab_avec_AL, "nb_obs", "nb_obs_alea")
spearman_test <- function(tableau_complet, nb_obs, nb_obs_pert) {
  
  
  obs <- tableau_complet[[nb_obs]]
  obs_pert <- tableau_complet[[nb_obs_pert]]
  
  test <- cor.test(obs,obs_pert, method = "spearman")
  
  rho <-test$estimate
  p_value <- test$p.value
  
  return(list(
    rho = rho,
    p_value = p_value
  ))
}







