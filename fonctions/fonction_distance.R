#' Title
#'
#' @param tableau_complet  tableau de données complet sous forme de dtaframe/datatable
#' @param n_obs nb d'observation originale
#' @param n_obs_pert nb d'observation perturbé
#'
#' @return list avec le calcul des distances
#' @export
#'
#' @examples
#' library(dplyr)
#' tableau_complet <- generer_tableau(100)
#' tab_avec_AL <- appliquer_arrondi_aleatoire(tableau_complet, 10)
#' calcul_distance(tab_avec_AL, "nb_obs", "nb_obs_alea")
calcul_distance <- function(tableau_complet, n_obs, n_obs_pert) {
  obs <- tableau_complet[[n_obs]]
  obs_pert <- tableau_complet[[n_obs_pert]]
  
  AAD <- mean(abs(obs - obs_pert))
  # L'AAD est plus intuitif et décrit la différence absolue moyenne par cellule non nulle d'une OA.
  
  HD <- mean(sqrt((1/2) * (sqrt(obs) - sqrt(obs_pert))^2))
  #La distance HD est basée sur la théorie de l'information. Il est fortement influencé par les petites cellules.
  
  RAD <- mean(abs(obs - obs_pert)/obs)
  
  return(list(AAD = AAD, HD = HD, RAD = RAD))
}  

