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
#' tableau_complet <- generer_tableau(100)
#' tab_avec_AL <- appliquer_arrondi_aleatoire(tableau_complet, 10)
#' liste_sous_tableaux <- recuperer_ts_sous_tableaux(
#'   tableau = tab_avec_AL,
#'   vars_cat = c("SEX","AGE","DIPL","REGION","DEPT")
#' )
#' str(liste_sous_tableaux)
#' # Exemple pour récupérer le tableau SEX * DIPL
#' liste_sous_tableaux$tabs_2Var$SEX_DIPL
recuperer_ts_sous_tableaux <- function(tableau, vars_cat, vars_num = "nb_obs", mod_total = "Total"){
  
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
              select(-all_of(v))
          }
          return(extract %>% select(all_of(c(combinaison, vars_num)))) 
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
