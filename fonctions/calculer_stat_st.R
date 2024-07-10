#' Title
#'
#' @param tableau tableau de données complet sous forme de dtaframe/datatable
#' @param vars_cat variable catégorielle du tableau
#' @param vars_num1 nom colonne obs originales
#' @param vars_num2 nom colonne obs perturbées par ckm
#' @param vars_num3 nom colonne obs perturbées par alea
#' @param mod_total "Total"
#'
#' @return
#' @export
#'
#' @examples
#' tableau_original <- generer_tableau(N)
#' tableau_perturbe <- appliquer_ckm(tableau_original, D, V)
#' tableau_perturbe <- appliquer_arrondi_aleatoire(tableau_perturbe, B)
#' vars_cat = c("SEX","AGE","DIPL","REGION","DEPT")
#' statistiques <- calculer_statistiques_sous_tableaux(tableau_perturbe, vars_cat, "nb_obs", "nb_obs_ckm", "nb_obs_alea", "Total")
#' str(statistiques)

calculer_statistiques_sous_tableaux <- function(tableau, vars_cat, vars_num1 = "nb_obs", vars_num2 = "nb_obs_ckm", vars_num3 = "nb_obs_alea", mod_total = "Total") {
  
  sous_tableaux <- recuperer_ts_sous_tableaux(tableau, vars_cat, vars_num1, vars_num2, vars_num3, mod_total)
  
  liste_resultats <- list()
  
  for (l in 1:length(sous_tableaux)) {
    for (name in names(sous_tableaux[[l]])) {
      sous_tableau <- sous_tableaux[[l]][[name]]
      distances_ckm <- calcul_distance(sous_tableau, vars_num1, vars_num2)
      distances_alea <- calcul_distance(sous_tableau, vars_num1, vars_num3)
      
      spearman_ckm <- spearman_test(sous_tableau, vars_num1, vars_num2)
      spearman_alea <- spearman_test(sous_tableau, vars_num1, vars_num3)
      
      wilcoxon_ckm <- wilcoxon_test(sous_tableau, vars_num1, vars_num2)
      wilcoxon_alea <- wilcoxon_test(sous_tableau, vars_num1, vars_num3)
      
      vr_ckm <- VR(sous_tableau, vars_num1, vars_num2)
      vr_alea <- VR(sous_tableau, vars_num1, vars_num3)
      
      taux_v_cramer_ckm <- NA
      taux_v_cramer_alea <- NA
      
      if (length(strsplit(name, "_")[[1]]) == 2) {
        
        tab_orig <- from_df_to_contingence(sous_tableau, vars_num1, vars_num2, vars_num3)
        tab_pert_ckm <- from_df_to_contingence(sous_tableau, vars_num2, vars_num1, vars_num3)
        tab_pert_alea <- from_df_to_contingence(sous_tableau, vars_num3, vars_num2, vars_num1)
        
        taux_v_cramer_ckm <- Taux_Variation_Vcramer(tab_orig, tab_pert_ckm)
        taux_v_cramer_alea <- Taux_Variation_Vcramer(tab_orig, tab_pert_alea)
      }
      
      taille_sous_tableau <- nrow(sous_tableau)
      nb_obs_inferieur_10 <- sum(sous_tableau[[vars_num1]] < 10)
      nb_obs_ckm_inferieur_10 <- sum(sous_tableau[[vars_num2]] < 10)
      nb_obs_alea_inferieur_10 <- sum(sous_tableau[[vars_num3]] < 10)
      
      df_resultats <- data.frame(
        Tableau = name,
        Taille = taille_sous_tableau,
        Nb_obs_inf_10 = nb_obs_inferieur_10,
        Nb_obs_inf_10_ckm = nb_obs_ckm_inferieur_10,
        Nb_obs_inf_10_alea = nb_obs_alea_inferieur_10,
        
        AAD_ckm = distances_ckm$AAD, AAD_alea = distances_alea$AAD,
        HD_ckm = distances_ckm$HD, HD_alea = distances_alea$HD,
        RAD_ckm = distances_ckm$RAD, RAD_alea = distances_alea$RAD,
        
        rho_Spearman_ckm = spearman_ckm$rho, rho_Spearman_alea = spearman_alea$rho,
        p_value_Spearman_ckm = spearman_ckm$p_value, p_value_Spearman_alea = spearman_alea$p_value,
        
        p_value_Wilcoxon_ckm = wilcoxon_ckm$p_value, p_value_Wilcoxon_alea = wilcoxon_alea$p_value,
        
        Taux_Variation_Variance_ckm = vr_ckm$vr, Taux_Variation_Variance_alea = vr_alea$vr,
        
        khi_deux_orig = taux_v_cramer_ckm$khi_deux_orig, 
        khi_deux_ckm = taux_v_cramer_ckm$khi_deux_pert, khi_deux_alea = taux_v_cramer_alea$khi_deux_pert,
        
        p_value_orig = taux_v_cramer_ckm$p_value_orig,
        p_value_ckm = taux_v_cramer_ckm$p_value_pert, p_value_alea = taux_v_cramer_alea$p_value_pert,
        
        V_Cramer_orig = taux_v_cramer_ckm$v_cramer_orig,
        V_Cramer_ckm = taux_v_cramer_ckm$vcramer_pert, V_Cramer_alea = taux_v_cramer_alea$vcramer_pert,
        Taux_Variation_V_Cramer_ckm = taux_v_cramer_ckm$vcramer_diff,Taux_V_Cramer_alea = taux_v_cramer_alea$vcramer$diff,
        
        stringsAsFactors = FALSE
      )
      liste_resultats <- bind_rows(liste_resultats, df_resultats)
    }
  }
  
  return(liste_resultats)
}
