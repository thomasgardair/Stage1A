
# Chargement des packages --------------------------------------------
library(dplyr)
library(data.table)
library(ggplot2)
library(ptable)
library(cellKey)
library(rtauargus)
library(purrr)
library(MASS)
library(data.table)
library(FactoMineR)
library(tibble)

#Paramètres fixes pour les deux méthodes

N = 10000
B=10
D=10
V=6.25


source("fonctions/fonction_arrondi_aleatoire.R")
source("fonctions/fonction_ckm.R")
source("fonctions/fonction_generer_tableau.R")
source("fonctions/fonction_distance.R")
source("fonctions/Wilcoxon.R")
source("fonctions/Spearman.R")
source("fonctions/Variance.R")
source("fonctions/Frequences marginales.R")
source("fonctions/fonction_afc.R")
source("fonctions/fonction_extraction.R")
source("fonctions/Log-lin.R")
source("fonctions/Vcramer.R")
source("fonctions/Tableau_contingence.R")
set.seed(40889)
# 1- Générer les micro

tableau_original <- generer_tableau(N)
str(tableau_original)

# 2- Appliquer la CKM

tableau_perturbe <- appliquer_ckm(tableau_original, D, V)
str(tableau_perturbe)


# 3- Appliquer AL
tableau_perturbe <- appliquer_arrondi_aleatoire(tableau_perturbe, B)
str(tableau_perturbe)

# 4- Distance
calcul_distance(tableau_perturbe, "nb_obs", "nb_obs_ckm")
calcul_distance(tableau_perturbe, "nb_obs", "nb_obs_alea")

# 5- Test Spearman
spearman_test(tableau_perturbe,"nb_obs","nb_obs_alea")
spearman_test(tableau_perturbe,"nb_obs","nb_obs_ckm")

# 6- Test Wilcoxon
wilcoxon_test(tableau_perturbe, "nb_obs","nb_obs_alea")
wilcoxon_test(tableau_perturbe, "nb_obs","nb_obs_ckm")


# 7- Variance
VR(tableau_perturbe,"nb_obs","nb_obs_alea")
VR(tableau_perturbe,"nb_obs","nb_obs_ckm")

# 8- Sous tableaux

liste_sous_tableaux_orig <- recuperer_ts_sous_tableaux(
  tableau = tableau_perturbe,
  vars_cat = c("SEX","AGE","DIPL","REGION","DEPT")
 )
liste_sous_tableaux_alea <- recuperer_ts_sous_tableaux(
  tableau = tableau_perturbe,
  vars_cat = c("SEX","AGE","DIPL","REGION","DEPT"),
  vars_num = "nb_obs_alea", mod_total = "Total"
)

tableau_orig <- liste_sous_tableaux_orig$tabs_2Var$DIPL_REGION
tableau_pert <- liste_sous_tableaux_alea$tabs_2Var$DIPL_REGION

# 9- Tableau contingence

tab_orig <- from_df_to_contingence(tableau_orig)
tab_pert <- from_df_to_contingence(tableau_pert,"nb_obs_alea")

# 10- Vcramer

Taux_Variation_Vcramer(tab_orig,tab_pert)

#11- acf
afc(tab_orig,tab_pert)

#12- Rapport de vraisemblance 
RV(tab_orig,tab_pert,~DIPL + REGION )



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
      
      df_resultats <- data.frame(
        Tableau = name,
        AAD_ckm = distances_ckm$AAD, HD_ckm = distances_ckm$HD, RAD_ckm = distances_ckm$RAD,
        AAD_alea = distances_alea$AAD, HD_alea = distances_alea$HD, RAD_alea = distances_alea$RAD,
        Spearman_ckm = spearman_ckm,
        Spearman_alea = spearman_alea,
        Wilcoxon_ckm = wilcoxon_ckm,
        Wilcoxon_alea = wilcoxon_alea,
        VR_ckm = vr_ckm,
        VR_alea = vr_alea,
        Taux_V_Cramer_ckm = taux_v_cramer_ckm,
        Taux_V_Cramer_alea = taux_v_cramer_alea,
        stringsAsFactors = FALSE
      )
      liste_resultats <- bind_rows(liste_resultats, df_resultats)
    }
  }
  
  return(liste_resultats)
}

vars_cat = c("SEX","AGE","DIPL","REGION","DEPT")
statistiques <- calculer_statistiques_sous_tableaux(tableau_perturbe, vars_cat, "nb_obs", "nb_obs_ckm", "nb_obs_alea", "Total")
print(statistiques)

