#' Title
#'
#' @param tableau 
#' @param D 
#' @param V 
#'
#' @return
#' @export
#'
#' @examples
#' library(dplyr)
#' library(data.table)
#' tableau_complet <- generer_tableau(100)
#' tab_avec_CKM <- appliquer_ckm(tableau_complet, 10, 6.25)
#' str(tab_avec_CKM)
appliquer_ckm <- function(tableau, D, V) {

  p_table <- ptable::create_cnt_ptable(D = D, V = V)
  
  tableau[
    ,
    `:=`(
      cell_key = rkeys_tot %% 1, # on récupère la partie décimale de la somme des clés
      i = ifelse(nb_obs <= D, nb_obs, D) #par commodité pour la fusion
      #les probas de transition pour les valeurs > D sont identiques à i = D
    )
  ]
  tableau[, cell_key_end := cell_key]
  setkey(tableau, i, cell_key, cell_key_end)
  
  table_transition <- p_table@pTable[, .(i,v,p_int_lb,p_int_ub)]
  setkey(table_transition, i, p_int_lb, p_int_ub)
  
  res <- foverlaps(tableau, table_transition, mult = "all")
  
  if (nrow(res) == nrow(tableau) & 
      nrow(res[cell_key > p_int_ub | cell_key < p_int_lb,]) == 0) {
    tableau <- res
  } else {
    stop("Erreur lors de la fusion")
  }
  
  tableau <- tableau %>% 
    as_tibble() %>% 
    mutate(nb_obs_ckm = nb_obs + v) %>% 
    select(-cell_key_end, -cell_key, -rkeys_tot, -i, -v, -p_int_lb, -p_int_ub)
  
  return(tableau)
}
