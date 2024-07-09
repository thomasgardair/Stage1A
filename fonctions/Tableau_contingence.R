#' Title
#'
#' @param tableau tableau sous forme de dataframe
#' @param freq_var nom colonne à selectionner
#' @param mod_total Total
#'
#' @return tableau de contingence
#' @export
#'
#' @examples
#' library(dplyr)
#' tableau_complet <- generer_tableau(100)
#' tab_avec_AL <- appliquer_arrondi_aleatoire(tableau_complet, 10)
#' liste_sous_tableaux_orig <- recuperer_ts_sous_tableaux(
#'   tableau = tab_avec_AL,
#'   vars_cat = c("SEX","AGE","DIPL","REGION","DEPT)
#' )
#' tableau_orig <- liste_sous_tableaux_orig$tabs_2Var$SEX_DIPL
#' from_df_to_contingence(tableau_orig)
from_df_to_contingence <- function(tableau, freq_var =  "nb_obs", mod_total = "Total"){
  
  # tableau <- liste_sous_tableaux$tabs_2Var$SEX_DIPL
  cat_vars <- tableau %>% select(where(is.character)) %>% names()
  
  tab <- tableau %>% 
    filter(.data[[cat_vars[1]]] != mod_total) %>%  
    filter(.data[[cat_vars[2]]] != mod_total) %>%  
    tidyr::pivot_wider(names_from = 2, values_from = freq_var, values_fill = 0) 
  
  mod_var1 <- tab %>% pull(1) 
  tab <- tab %>% select(-1) %>% as.matrix()
  rownames(tab) <- mod_var1
  
  return(tab)
}