#' Title
#'
#' @param table_contingence_orig tableau contingence avec données originales
#' @param table_contingence_pert tableau contingence avec données perturbées
#'
#' @return liste avec afc et test khi-deux 
#' @export
#'
#' @examples
#' #' library(dplyr)
#' tableau_complet <- generer_tableau(100)
#' tab_avec_AL <- appliquer_arrondi_aleatoire(tableau_complet, 10)
#' liste_sous_tableaux_orig <- recuperer_ts_sous_tableaux(
#'   tableau = tab_avec_AL,
#'   vars_cat = c("SEX","AGE","DIPL","REGION")
#' )
#' liste_sous_tableau_pert <- recuperer_ts_sous_tableaux(
#' tableau = tab_avec_AL,
#' vars_cat = c("SEX","AGE","DIPL","REGION"),
#' vars_num = "nb_obs_alea", mod_total = "Total"
#' )
#' 
#' afc(liste_sous_tableaux_orig$tabs_2Vars$SEX_DIPL,liste_sous_tableaux_pert$tabs_2Var$SEX_DIPL)
afc <- function(table_contingence_orig, table_contingence_pert){
  
  afc_orig <- CA(table_contingence_orig, graph = FALSE)
  afc_pert <- CA(table_contingence_pert, graph = FALSE)

  coords_row <- afc_orig$row$coord[,c(1,2)] %>% as.data.frame() %>% 
    rename(x = `Dim 1`, y = `Dim 2`) %>% 
    mutate(REGION = rownames(afc_orig$row$coord), TYPE="orig") %>% 
    bind_rows(
      afc_pert$row$coord[,c(1,2)] %>% as.data.frame() %>% 
        rename(x = `Dim 1`, y = `Dim 2`) %>% 
        mutate(REGION = rownames(afc_pert$row$coord),TYPE="pert"))

  plot_row <- coords_row %>% 
    ggplot() +
    geom_point(aes(x=x, y=y, color = TYPE)) +
    ggrepel::geom_text_repel(aes(x=x, y=y, color = TYPE, label = REGION))+
    labs(
      title = "AFC ROW",
      x = "Dim 1",
      y = "Dim 2",
      color= "Type"
    )+
    theme_minimal()+
    geom_hline(yintercept = 0, color = "black", linetype="dashed") +
    geom_vline(xintercept = 0, color = "black",linetype="dashed")

  coords_col <- afc_orig$col$coord[,c(1,2)] %>% as.data.frame() %>% 
    rename(x = `Dim 1`, y = `Dim 2`) %>% 
    mutate(AGE = rownames(afc_orig$col$coord), TYPE="orig") %>% 
    bind_rows(
      afc_pert$col$coord[,c(1,2)] %>% as.data.frame() %>% 
        rename(x = `Dim 1`, y = `Dim 2`) %>% 
        mutate(AGE = rownames(afc_pert$col$coord),TYPE="pert"))

  plot_col <- coords_col %>% 
    ggplot() +
    geom_point(aes(x=x, y=y, color = TYPE)) +
    ggrepel::geom_text_repel(aes(x=x, y=y, color = TYPE, label = AGE))+
    labs(
      title = "AFC COL",
      x = "Dim 1",
      y = "Dim 2",
      color= "Type"
    )+
    theme_minimal()+
    geom_hline(yintercept = 0, color = "black", linetype="dashed") +
    geom_vline(xintercept = 0, color = "black",linetype="dashed")
  
  return(list(plot_row = plot_row, plot_col = plot_col, test_orig = test_orig, test_pert = test_pert))
}






