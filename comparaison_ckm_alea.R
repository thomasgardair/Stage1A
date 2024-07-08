
# Chargement des packages --------------------------------------------
library(dplyr)
library(data.table)
library(ggplot2)
library(ptable)
library(cellKey)
library(rtauargus)



#Paramètres fixes pour les deux méthodes

N = 10000
B=10
D=10
V=6.25


source("fonctions/fonction_arrondi_aleatoire.R")
source("fonctions/fonction_distance.R")
source("fonctions/fonction_ckm.R")
source("fonctions/fonction_generer_tableau.R")


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