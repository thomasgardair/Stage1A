
# Chargement des packages --------------------------------------------
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
library(dplyr)

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
source("fonctions/calculer_stat_st.R")
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

#12- Tout en 1

vars_cat = c("SEX","AGE","DIPL","REGION","DEPT")

resultats <- calculer_statistiques_sous_tableaux(tableau_perturbe, vars_cat, "nb_obs", "nb_obs_ckm", "nb_obs_alea", "Total")
statistiques <- resultats$statistiques
liste_afc <- resultats$afc

