#' Title
#'
#' @param tableau tableau de données complet sous forme de dtaframe/datatable
#' @param vars_cat variable du tableau
#' @param vars_num nom colonne nb obs
#' @param mod_total nom mod_total : "Total"
#'
#' @return liste de liste avec les sous tableaux 
#' @export
#'
#' @examples
#' library(dplyr)
#' tableau_original <- generer_tableau(N)
#' tableau_perturbe <- appliquer_ckm(tableau_original, D, V)
#' tableau_perturbe <- appliquer_arrondi_aleatoire(tableau_perturbe, B)
#' liste_sous_tableaux <- recuperer_ts_sous_tableaux(
#'   tableau = tableau_perturbe,
#'   vars_cat = c("SEX","AGE","DIPL","REGION","DEPT")
#' )
#' str(liste_sous_tableaux)
#' # Exemple pour récupérer le tableau SEX * DIPL
#' liste_sous_tableaux$tabs_2Var$SEX_DEPT
recuperer_ts_sous_tableaux <- function(tableau, vars_cat, vars_num1 = "nb_obs", vars_num2= "nb_obs_ckm",vars_num3 = "nb_obs_alea", mod_total = "Total"){
  
  require(purrr)
  combinaison_vars <- purrr::map(1:length(vars_cat), ~combn(vars_cat, .))
  
  liste_sous_tab_contingence <- list()
  
  for(l in 1:length(vars_cat)){
    comb_vars <- combn(vars_cat, l)
    
    liste_sous_tab_contingence[[l]] <- 
      purrr::map(
        1:ncol(comb_vars),
        \(i){
          combinaison = comb_vars[,i]
          nocomb = vars_cat[ ! vars_cat %in% combinaison]
          extract <- tableau
          for(v in nocomb){
            extract <- extract %>% 
              filter(.data[[v]] == mod_total) %>% 
              dplyr::select(-all_of(v))
          }
          return(extract %>% dplyr::select(all_of(c(combinaison, vars_num1, vars_num2, vars_num3)))) 
        }
      )
    names(liste_sous_tab_contingence[[l]]) <- purrr::map(
      1:ncol(comb_vars), 
      ~paste0(comb_vars[,.], collapse = '_')
    )
  }
  
  names(liste_sous_tab_contingence) <- paste0("tabs_", 1:length(vars_cat), "Var")
  
  return(liste_sous_tab_contingence)
}
