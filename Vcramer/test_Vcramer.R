# Installation des packages -------------------------------------------

install.packages("remotes")
remotes::install_github(
  "InseeFrLab/rtauargus",
  build_vignettes = FALSE,
  upgrade = "never"
)

install.packages("ptable")
install.packages("cellKey")


# Chargement des packages --------------------------------------------
library(dplyr)
library(data.table)
library(ggplot2)
library(ptable)
library(cellKey)
library(rtauargus)

# Paramètres ------------------------------------------
N = 100000 # Nombre d'individus dans la table
D = 5 # Déviation de la CKM
V = 6.25 # Variance de la CKM

seed = 40889 # graine aléatoire pour reproduire le jeu

# Fonction pour générer les tableaux et calculer le V de Cramer -----------------------------------
appliquer_ckm <- function(seed) {
  set.seed(seed)
  
  # 0-a Créer/Importer la table de données individuelles -----------------------------------
  micro_data <- tibble(
    SEX = sample(c("H","F"), N, replace = TRUE, prob = c(0.48,0.52)),
    DIPL = sample(
      c("CAP", "BAC", "LIC", "MAST"), N, replace = TRUE, prob = c(0.1,0.5,0.3,0.1)),
    AGE = sample(seq(0,100,10), N, replace = TRUE),
    REGION = sample(1:13, N, replace = TRUE)
  )
  
  # 0-b Tirage des clés aléatoires individuelles -----------------------------------
  micro_data$rkeys <- cellKey::ck_generate_rkeys(dat = micro_data, nr_digits = 5+log(N)/log(10))
  
  # 0-a La matrice de transition -----------------------------------
  p_table <- ptable::create_cnt_ptable(D = D, V = V)
  
  # 1-a Construire un tableau -----------------------------------
  tableau_complet <- rtauargus::tabulate_micro_data( 
    micro_data,
    cat_vars = c("SEX", "DIPL", "AGE", "REGION"),
    resp_var = "rkeys", 
    marge_label = "Total"
  )
  
  # 1-b On prépare la table à la fusion -----------------------------------
  tableau_complet[
    ,
    `:=`(
      rkeys_max = NULL, # on supprime la colonne rkeys_max pas utile pour nous
      cell_key = rkeys_tot %% 1, # on récupère la partie décimale de la somme des clés
      i = ifelse(nb_obs <= p_table@pParams@D, nb_obs, p_table@pParams@D) #par commodité pour la fusion
      #les probas de transition pour les valeurs > D sont identiques à i = D
    )
  ]
  # 2- Associer la perturbation pour chaque cellule
  tableau_complet[, cell_key_end := cell_key]
  setkey(tableau_complet, i, cell_key, cell_key_end)
  
  table_transition <- p_table@pTable[, .(i,v,p_int_lb,p_int_ub)]
  setkey(table_transition, i, p_int_lb, p_int_ub)
  
  res <- foverlaps(tableau_complet, table_transition, mult = "all")
  
  if (nrow(res) == nrow(tableau_complet) & 
      nrow(res[cell_key > p_int_ub | cell_key < p_int_lb,]) == 0) {
    tableau_complet <- res
  } else {
    stop("Erreur lors de la fusion")
  }
  
  # 3- Perturbation = Calcul de la valeur finale -----------------------------------
  tableau_complet <- tableau_complet %>% 
    as_tibble() %>% 
    mutate(nb_obs_pert = nb_obs + v)
  
  # 4- Analyse avant après --------------------------
  
  # 4-a Calcul de la moyenne des écarts absolus -----------------------------------
  MAE <- mean(abs(tableau_complet$nb_obs - tableau_complet$nb_obs_pert))
  
  # 4-b Calcul du V de Cramer -----------------------------------
  Vcramer <- function(tab) {
    if (!is.table(tab)) {
      stop("L'argument doit être une table de contingence.")
    }
    chi2 <- chisq.test(tab)$statistic
    n <- sum(tab)  
    k <- min(dim(tab)) - 1
    vcramer <- sqrt(chi2 / (n * k))
    return(as.numeric(vcramer))
  }
  
  tab_sans_marges <- tableau_complet %>% filter(REGION != "Total" & AGE != "Total" & SEX != "Total" & DIPL != "Total")
  
  original_tab <- xtabs(nb_obs ~ REGION + AGE, data = tab_sans_marges)
  perturbed_tab <- xtabs(nb_obs_pert ~ REGION + AGE, data = tab_sans_marges)
  
  vcramer_original <- Vcramer(original_tab)
  vcramer_perturbed <- Vcramer(perturbed_tab)
  
  vcramer_diff <- abs(vcramer_original - vcramer_perturbed)
  
  return(vcramer_diff)
}

# Effectuer 100 simulations
set.seed(seed)
simulation_results <- replicate(100, appliquer_ckm(sample(1:100000, 1)))

# Résultats
summary(simulation_results)
hist(simulation_results, breaks = 30, main = "Distribution des différences de V de Cramer", xlab = "Différence de V de Cramer")





