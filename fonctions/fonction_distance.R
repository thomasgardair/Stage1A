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
calcul_distance <- function(tableau_complet, n_obs, n_obs_pert, mod_total) {
  
  columns <- colnames(tableau_complet)
  columns <- columns[grepl("^[A-Z_]+$", columns)]
  
  tableau_complet <- tableau_complet %>%  filter(if_all(all_of(columns), ~ . != mod_total)) 
  
  obs <- as.numeric(tableau_complet[[n_obs]])
  obs_pert <- as.numeric(tableau_complet[[n_obs_pert]])
  
  AAD <- mean(abs(obs - obs_pert))
  # L'AAD est plus intuitif et décrit la différence absolue moyenne par cellule non nulle d'une OA.
  
  HD <- mean(sqrt((1/2) * (sqrt(obs/sum(obs)) - sqrt(obs_pert/sum(obs_pert)))^2))
  #La distance HD est basée sur la théorie de l'information. Il est fortement influencé par les petites cellules.
  
  RAD <- mean(abs(obs - obs_pert)/obs)
  
  return(list(AAD = AAD, HD = HD, RAD = RAD))
}  

