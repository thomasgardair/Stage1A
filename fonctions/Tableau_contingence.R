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
#' tableau_original <- generer_tableau(N)
#' tableau_perturbe <- appliquer_ckm(tableau_original, D, V)
#' tableau_perturbe <- appliquer_arrondi_aleatoire(tableau_perturbe, B)
#' liste_sous_tableaux <- recuperer_ts_sous_tableaux(
#'   tableau = tableau_perturbe,
#'   vars_cat = c("SEX","AGE","DIPL","REGION","DEPT")
#' )
#' str(liste_sous_tableaux)
#' # Exemple pour récupérer le tableau SEX * DIPL
#' liste_sous_tableaux$tabs_2Var$SEX_AGE
#' tableau_orig <- liste_sous_tableaux$tabs_2Var$SEX_AGE
#' from_df_to_contingence(tableau_orig)
#' from_df_to_contingence(tableau_orig,"nb_obs_ckm","nb_obs","nb_obs_alea")
from_df_to_contingence <- function(tableau, freq_var1 =  "nb_obs", freq_var2 =  "nb_obs_ckm", freq_var3 =  "nb_obs_alea", mod_total = "Total"){
  
  tableau <- tableau %>% dplyr::select(-freq_var2, -freq_var3)
  cat_vars <- tableau %>% dplyr::select(where(is.character)) %>% names()
  
  tab <- tableau %>% 
    filter(.data[[cat_vars[1]]] != mod_total) %>%  
    filter(.data[[cat_vars[2]]] != mod_total) %>%  
    tidyr::pivot_wider(names_from = 2, values_from = freq_var1, values_fill = 0) 
  
  mod_var1 <- tab %>% pull(1) 
  tab <- tab %>% dplyr::select(-1) %>% as.matrix()
  rownames(tab) <- mod_var1
  
  return(tab)
}
