
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
library(tidyr)

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
source("fonctions/plot.R")
set.seed(40889)
# 1- Générer les micro

tableau_original <- generer_tableau(N)
str(tableau_original)

#1- Appeler les micro

data <- "X:/HAB-INVEST-CONFIDENTIALITE/QPV/Pole_Emploi/liste_tableaux_pole_emploi_avant_pert.rds" 

data2 <- "X:/HAB-INVEST-CONFIDENTIALITE/QPV/Pole_Emploi/tableau_PE_6VARS.RDS"

liste_tableaux <-readRDS(data)

str(liste_tableaux)

tableau_1 <-liste_tableaux[[1]]
tableau_1 <- tableau_1 %>% select(-rkeys_max,-ck) %>%rename(PLGQP = PLG_QP)

tableau_2 <-liste_tableaux[[2]]
tableau_2 <- tableau_2 %>% select(-rkeys_max,-ck)%>%rename(PLGQP = PLG_QP)%>%rename(AGE3c = AGE_3c)

tableau_3 <-liste_tableaux[[3]]
tableau_3 <- tableau_3 %>% select(-rkeys_max,-ck)%>%rename(PLGQP = PLG_QP)

tableau_4 <-liste_tableaux[[4]]
tableau_4 <- tableau_4 %>% select(-rkeys_max,-ck)%>%rename(PLGQP = PLG_QP)

tableau_5 <-liste_tableaux[[5]]
tableau_5 <- tableau_5 %>% select(-rkeys_max,-ck)%>%rename(PLGQP = PLG_QP)

# 2- Appliquer la CKM

tableau_perturbe <- appliquer_ckm(tableau_1, D, V)
str(tableau_perturbe)


# 3- Appliquer AL
tableau_perturbe <- appliquer_arrondi_aleatoire(tableau_perturbe, B)
str(tableau_perturbe)


#4- Tout en 1


vars_cats = c("CATEG","PLGQP","SEXE")

resultats <- calculer_statistiques_sous_tableaux(tableau_perturbe, vars_cats, "nb_obs", "nb_obs_ckm", "nb_obs_alea", "Ensemble")
statistiques <- resultats$statistiques
plot_afc <- resultats$afc
plot_distances <- resultats$plot_distances




# Lancer le calcul des stats sur tous les tableaux

liste_resultats <- liste_tableaux %>% 
  purrr::map(
    \(tab){
      tableau_perturbe <- appliquer_ckm(tab, D, V) %>% 
        appliquer_arrondi_aleatoire(B)
      
      tableau_perturbe <- tableau_perturbe %>% rename(PLGQP = PLG_QP)
      if("AGE_3c" %in% names(tableau_perturbe)) 
        tableau_perturbe <- tableau_perturbe %>% rename(AGE3c = AGE_3c)
      vars_cat <- tableau_perturbe %>% select(where(is.character)) %>% names()
      res <- calculer_statistiques_sous_tableaux(
        tableau_perturbe %>% select(-rkeys_max,-ck), vars_cat, 
        "nb_obs", "nb_obs_ckm", "nb_obs_alea", "Ensemble"
      )
    }, 
    .progress = TRUE
  )

all_statistiques <- purrr::map(liste_resultats, \(r) r$statistiques) %>%
  purrr::list_rbind()

rownames(all_statistiques) <- NULL

all_statistiques <- all_statistiques %>% 
  group_by(across(-ends_with("_alea"))) %>% 
  summarise(
    across(ends_with("_alea"), mean, na.rm = TRUE),
    .groups = "drop"
  )
  
ggplot(all_statistiques) +
  geom_point(aes(x = Taille, y = HD_ckm))

ggplot(all_statistiques) +
  geom_point(aes(x = Taille, y = HD_ckm, color = 'HD_ckm')) +
  geom_point(aes(x = Taille, y = HD_alea, color = 'HD_alea')) +
  labs(title = "Superposition des points HD_ckm et HD_alea",
       x = "Nombre de cellules",
       y = "Distances Hellinger",
       color = "Légende") +
  theme_bw()
