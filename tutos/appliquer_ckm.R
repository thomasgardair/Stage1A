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
D = 10 # Déviation de la CKM
V = 6.25 # Variance de la CKM

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

str(micro_data)
summary(micro_data)

# 0-b Tirage des clés aléatoires individuelles -----------------------------
micro_data$rkeys <- cellKey::ck_generate_rkeys(dat = micro_data, nr_digits = 5+log(N)/log(10))

# On peut vérifier que la distribution empirique des clés suit une loi uniforme
hist(micro_data$rkeys, breaks = 20)

# 0-a La matrice de transition -------------------------------------------

p_table <- ptable::create_cnt_ptable(D = D, V = V)
p_table@empResults[-1,]
plot(p_table, type = "p")
plot(p_table, type = "d")

str(p_table@pTable)



# 1-a Construire un tableau ----------------------------------------------
# On construit ici le tableau croisant toutes les variables 
# Avec toutes les marges 
# Ainsi on pourra considérer des sous-tableaux selon les analyses qu'on veut faire
tableau_complet <- rtauargus::tabulate_micro_data( # fonction d'agrégation
  micro_data,
  cat_vars = c("SEX", "DIPL", "AGE", "REGION"),
  resp_var = "rkeys", #pour agréger les clés en même temps que de réaliser les comptages
  marge_label = "Total"
)

str(tableau_complet)

# RQ: le tableau généré est de classe "data.table" 
#classe qu'on conserve le temps de faire la fusion

# 1-b On prépare la table à la fusion --------------------------------
tableau_complet[
  ,
  `:=`(
    rkeys_max = NULL, # on supprime la colonne rkeys_max pas utile pour nous
    cell_key = rkeys_tot %% 1, # on récupère la partie décimale de la somme des clés
    i = ifelse(nb_obs <= p_table@pParams@D, nb_obs, p_table@pParams@D) #par commodité pour la fusion
    #les probas de transition pour les valeurs > D sont identiques à i = D
  )
]

# Vérification de l'uniformité empirique de cell_key (clés des cellules) --
summary(tableau_complet$cell_key)
hist(tableau_complet$cell_key)

# 2- associer la perturbation pour chaque cellule --------------------------

tableau_complet[,cell_key_end := cell_key]
setkey(tableau_complet, i, cell_key, cell_key_end)

table_transition <- p_table@pTable[,.(i,v,p_int_lb,p_int_ub)]
setkey(table_transition, i, p_int_lb, p_int_ub)

res <- foverlaps(
  tableau_complet,
  table_transition,
  mult = "all"
)

if(nrow(res) == nrow(tableau_complet) & 
   nrow(res[cell_key > p_int_ub | cell_key < p_int_lb,]) == 0){
  tableau_complet <- res
}else{
  print("Erreur lors de la fusion")
}

str(tableau_complet)

tableau_complet

# Vérification des probas empiriques de transition
p_emp <- merge(
  tableau_complet[, .N, by = .(i,v)][, `:=`(p_emp = N/sum(N)), by = .(i)],
  p_table@pTable[, .(i,v,p)],
  by = c("i","v")
)
p_emp[, p_diff := abs(p - p_emp)][]
p_emp[order(p_diff, decreasing = TRUE),]
p_emp[, quantile(p_diff, seq(0,1,0.1))]


ggplot(p_emp) +
  geom_line(aes(x = v, y = p), color = "darkred") +
  geom_line(aes(x = v, y = p_emp), color = "steelblue")

# 3- Pertubation = Calcul la valeur finale -----------------------------------

tableau_complet <- tableau_complet %>% 
  as_tibble() %>% 
  mutate(nb_obs_pert = nb_obs + v)

str(tableau_complet)

# 4- Analyse avant après --------------------------

#4-a Calcul de la moyenne des écarts absolus -----------------


MAE <- mean(abs(tableau_complet$nb_obs - tableau_complet$nb_obs_pert))

