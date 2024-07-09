
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

#6- Test Wilcoxon
wilcoxon_test(tableau_perturbe, "nb_obs","nb_obs_alea")
wilcoxon_test(tableau_perturbe, "nb_obs","nb_obs_ckm")


#7- Variance
VR(tableau_perturbe,"nb_obs","nb_obs_alea")
VR(tableau_perturbe,"nb_obs","nb_obs_ckm")



set.seed(123) 
simuler_arrondi_aleatoire <- replicate(100, appliquer_arrondi_aleatoire(sample(1:100000, 1)), simplify = FALSE)

#Calcul des distances pour chaque tableau

distance_arrondi_aleatoire <- bind_rows(lapply(seq_along(simuler_arrondi_aleatoire), function(i) {
  tableau <- simuler_arrondi_aleatoire[[i]]
  res <- calcul_distance(tableau, "nb_obs", "nb_obs_alea")
  data.frame(
    AAD = res$AAD,
    HD = res$HD,
    RAD = res$RAD
  )
}))

#Moyenne des distances

distance_arrondi_aleatoire %>%
  summarize(mean(AAD),mean(HD), mean(RAD))

#Méthode CKM

#Appliquer la méthode à 1 tableau



#Appliquer la méthode à 100 tableaux

set.seed(123) 
simuler_ckm <- replicate(100, appliquer_ckm(sample(1:100000, 1)), simplify = FALSE)

#Calcul des distances pour chaque tableau

distance_ckm <- bind_rows(lapply(seq_along(simuler_ckm), function(i) {
  tableau <- simuler_ckm[[i]]
  res <- calcul_distance(tableau, "nb_obs", "nb_obs_pert")
  data.frame(
    AAD = res$AAD,
    HD = res$HD,
    RAD = res$RAD
  )
}))

#Moyenne des distances

distance_ckm %>%
  summarize(mean(AAD),mean(HD), mean(RAD))

#Calcul des tests pour chaque tableau

Spearman_arrondi_aleatoire <- bind_rows(lapply(seq_along(simuler_arrondi_aleatoire), function(i) {
  tableau <- simuler_arrondi_aleatoire[[i]]
  res <- spearman_test(tableau, "nb_obs", "nb_obs_alea")
  data.frame(
    S = res$statistic,
    rho = res$estimate,
    p_value = res$p.value
  )
}))

#Moyenne des stats

Spearman_arrondi_aleatoire %>%
  summarize(mean(S),mean(rho),mean(p_value))