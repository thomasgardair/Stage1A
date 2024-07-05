
#' Title
#'
#' @param tableau le tableau complet à découper
#' @param vars_cat vecteur des noms de variables catégorielles à partir 
#' desquelles sont construits les sous-tableaux
#' @param vars_num variables numériques (et autres) à récupérer dans les ss-tableaux
#' par défaut "nb_obs".
#' @param mod_total Modalité du total (par défaut "Total")
#'
#' @return liste de listes de tableaux ordonnées par largeur des tableaux
#' @export
#'
#' @examples
#'
#' # Chargement des packages --------------------------------------------
#' library(dplyr)
#' library(purrr)
#' library(data.table)
#' library(ggplot2)
#' library(rtauargus)
#' 
#' 
#' # Paramètres ------------------------------------------
#' N = 10000 # nb individus
#' seed = 40889 # graine aléatoire pour reproduire le jeu
#' set.seed(seed)
#' 
#' # 0-a Créer/Importer la table de données individuelles -------------------
#' 
#' micro_data <- tibble(
#'   SEX = sample(c("H","F"), N, replace = TRUE, prob = c(0.48,0.52)),
#'   DIPL = sample(
#'     c("CAP", "BAC", "LIC", "MAST"), N, replace = TRUE, prob = c(0.1,0.5,0.3,0.1)),
#'   AGE = sample(seq(0,100,10), N, replace = TRUE),
#'   REGION = sample(1:13, N, replace = TRUE)
#' )
#' 
#' str(micro_data)
#' summary(micro_data)
#' 
#' tableau_complet <- rtauargus::tabulate_micro_data( # fonction d'agrégation
#'   micro_data,
#'   cat_vars = c("SEX", "DIPL", "AGE", "REGION"),
#'   resp_var = NULL, #pour agréger les clés en même temps que de réaliser les comptages
#'   marge_label = "Total"
#' )
#' 
#' 
#' vars_cat  = 
#' vars_num = "nb_obs"
#'
#' liste_sous_tableaux <- recuperer_ts_sous_tableaux(
#'   tableau = tableau_complet,
#'   vars_cat = c("SEX","AGE","DIPL","REGION")
#' )
#' str(liste_sous_tab_contingence)
#' # Exemple pour récupérer le tableau SEX * DIPL
#' liste_sous_tab_contingence$tabs_2Var$SEX_DIPL
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
          extract <- tableau_complet 
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


