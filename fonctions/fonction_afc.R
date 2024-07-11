#' Title
#'
#' @param table_contingence_orig tableau contingence avec données originales
#' @param table_contingence_pert tableau contingence avec données perturbées
#'
#' @return liste avec afc et test khi-deux 
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
#' liste_sous_tableaux$tabs_2Var$SEX_DIPL
#' tableau_orig <- liste_sous_tableaux$tabs_2Var$AGE_DIPL
#' tab_orig <- from_df_to_contingence(tableau_orig)
#' tab_pert <- from_df_to_contingence(tableau_orig,"nb_obs_ckm","nb_obs","nb_obs_alea")
#' afc(tab_orig,tab_pert)
afc <- function(table_contingence_orig, table_contingence_pert){
  
  if (ncol(table_contingence_orig) < 3 | nrow(table_contingence_orig) < 3) {
    return(NULL)
  }
  
  if (ncol(table_contingence_pert) < 3 | nrow(table_contingence_pert) < 3) {
    return(NULL)
  }
  
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
  
  return(list(plot_row = plot_row, plot_col = plot_col))
}






