# Installation des packages -------------------------------------------

install.packages("remotes")
remotes::install_github(
  "InseeFrLab/rtauargus",
  build_vignettes = FALSE,
  upgrade = "never"
)


# Chargement des packages --------------------------------------------
library(dplyr)
library(data.table)
library(ggplot2)
library(rtauargus)


# Paramètres ------------------------------------------
B = 10 # base de l'arrondi
N = 100000 # nb individus
seed = 40889 # graine aléatoire pour reproduire le jeu
set.seed(seed)

# 0-a Créer/Importer la table de données individuelles -------------------

micro_data <- tibble(
  SEX = sample(c("H","F"), N, replace = TRUE, prob = c(0.48,0.52)),
  DIPL = sample(
    c("CAP", "BAC", "LIC", "MAST"), N, replace = TRUE, prob = c(0.1,0.5,0.3,0.1)),
  AGE = sample(seq(0,100,10), N, replace = TRUE),
  REGION = sample(1:13, N, replace = TRUE)
)
micro_data <- micro_data %>%
  mutate(DIPL = if_else(AGE < 20, "<BAC", DIPL))

departments <- c("a", "b", "c", "d")
micro_data <- micro_data %>%
  mutate(DEPT = paste(REGION, sample(departments, N, replace = TRUE), sep = "_"))

str(micro_data)
summary(micro_data)

# 1-a Construire un tableau ----------------------------------------------
# On construit ici le tableau croisant toutes les variables 
# Avec toutes les marges 
# Ainsi on pourra considérer des sous-tableaux selon les analyses qu'on veut faire
tableau_complet <- rtauargus::tabulate_micro_data( # fonction d'agrégation
  micro_data,
  cat_vars = c("SEX", "DIPL", "AGE", "REGION","DEPT"),
  resp_var = NULL, #pour agréger les clés en même temps que de réaliser les comptages
  marge_label = "Total"
)

str(tableau_complet)



tableau_complet <- tableau_complet %>% 
  # 2-a Pose des probabilités de transition pour chaque comptage
  mutate( 
    p_inf = (B - (nb_obs %% B))/B,
    p_sup = 1 - p_inf
  ) %>%
  # 2-b Les valeurs sup et inf possibles
  mutate(
    val_inf = floor(nb_obs/B)*B,
    val_sup = ceiling(nb_obs/B)*B
  )

tableau_complet <- tableau_complet %>% 
  # 2-c tirage de la valeur perturbée
  mutate(
    nb_obs_pert = apply(
      tableau_complet %>% select(p_inf, p_sup, val_inf, val_sup),
      MARGIN = 1,
      FUN = \(r) sample(
        c(r[["val_inf"]], r[["val_sup"]]), 
        size = 1, 
        prob = c(r[["p_inf"]], r[["p_sup"]])
      )
    )
  )


# 3- Comparaions des probabilités de transition empiriques aux théoriques

tableau_complet %>% 
  mutate(reste = nb_obs %% B) %>% 
  group_by(reste, p_inf, p_sup) %>% 
  summarise(
    p_inf_emp = mean(nb_obs_alea == val_inf),
    p_sup_emp = mean(nb_obs_alea == val_sup)
  )



