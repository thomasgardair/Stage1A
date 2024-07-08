#' Title
#'
#' @param tableau 
#' @param freq_var 
#' @param mod_total 
#'
#' @return
#' @export
#'
#' @examples
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