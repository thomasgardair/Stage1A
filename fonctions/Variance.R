

#' Title
#'
#' @param tableau_complet tableau de données complet sous forme de dtaframe/datatable
#' @param n_obs nom colonne obs originales 
#' @param n_obs_pert nom colonne obs perturbées
#'
#' @return liste avec la taux de variation de variance, variance original, variance perturbé
#' @export
#'
#' @examples
#' library(dplyr)
#' tableau_complet <- generer_tableau(100)
#' tab_avec_AL <- appliquer_arrondi_aleatoire(tableau_complet, 10)
#' VR(tab_avec_AL, "nb_obs", "nb_obs_alea")
VR <- function(tableau_complet, n_obs,n_obs_pert){
  
  obs <- tableau_complet[[n_obs]]
  obs_pert <- tableau_complet[[n_obs_pert]]
  
  V_orig <- mean((obs -(mean(obs)))^2 )
  V_pert <- mean((obs_pert - (mean(obs)))^2)
  
  vr <- 100*((V_pert - V_orig)/V_orig)
  
  return(list(vr = vr))
}



