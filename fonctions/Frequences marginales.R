#' Title
#'
#' @param tableau_complet  tableau de données complet sous forme de dtaframe/datatable
#' @param var la variable dont on veut la frequence marginale
#' @param nb_obs nom colonne nb d'observation originale
#' @param nb_obs_pert nom colonne nb d'observation perturbée
#'
#' @return frequence marginale sous forme de dataframe/datatable
#' @export
#'
#' @examples
#' library(dplyr)
#' tableau_complet <- generer_tableau(100)
#' tab_avec_AL <- appliquer_arrondi_aleatoire(tableau_complet, 10)
#' frequences_marginales(tab_avec_AL,"nb_obs","nb_obs_alea", "SEX")
frequences_marginales <- function(tableau_complet, nb_obs, nb_obs_alea, var){
  
  columns <- colnames(tableau_complet)
  columns <- columns[grepl("^[A-Z_]+$", columns)]
  columns <- setdiff(columns, var)
  
  freq <- tableau_complet %>%
    filter(if_all(all_of(columns), ~ . == "Total")) %>%
    select(all_of(var), nb_obs, nb_obs_alea)
  
  return(freq)
}

