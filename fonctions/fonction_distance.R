#Fonction pour calculer les distances entre les valeurs originales et perturbées

calcul_distance <- function(tableau_complet, n_obs, n_obs_pert) {
  if(!(n_obs %in% colnames(tableau_complet)) | !(n_obs_pert %in% colnames(tableau_complet))) {
    stop("Les colonnes  n'existent pas dans le tableau")
  }
  
  obs <- tableau_complet[[n_obs]]
  obs_pert <- tableau_complet[[n_obs_pert]]
  
  MAE <- mean(abs(obs - obs_pert))
  HD <- mean(sqrt((1/2) * (sqrt(obs) - sqrt(obs_pert))^2))
  
  return(list(MAE = MAE, HD = HD))
}  

#Paramètres fixes pour les deux méthodes

N = 10000
B=10
D=10
V=6.25
seed=40889

#Méthode d'arrondi aléatoire

#Appliquer la méthodes à 1 tableau

appliquer_arrondi_aleatoire<- function(seed) {
  set.seed(seed)
  
  micro_data <- tibble(
    SEX = sample(c("H", "F"), N, replace = TRUE, prob = c(0.48, 0.52)),
    DIPL = sample(c("CAP", "BAC", "LIC", "MAST"), N, replace = TRUE, prob = c(0.1, 0.5, 0.3, 0.1)),
    AGE = sample(seq(0, 100, 10), N, replace = TRUE),
    REGION = sample(1:13, N, replace = TRUE)
  )
  
  tableau_complet <- rtauargus::tabulate_micro_data(
    micro_data,
    cat_vars = c("SEX", "DIPL", "AGE", "REGION"),
    resp_var = NULL,
    marge_label = "Total"
  )
  
  tableau_complet <- tableau_complet %>%
    mutate(
      p_inf = (B - (nb_obs %% B))/B,
      p_sup = 1 - p_inf,
      val_inf = floor(nb_obs/B)*B,
      val_sup = ceiling(nb_obs/B)*B
    )
  tableau_complet <- tableau_complet %>% 
    mutate(
      nb_obs_alea = apply(
        tableau_complet %>% select(p_inf, p_sup, val_inf, val_sup),
        MARGIN = 1,
        FUN = \(r) sample(
          c(r[["val_inf"]], r[["val_sup"]]), 
          size = 1, 
          prob = c(r[["p_inf"]], r[["p_sup"]])
        )
      )
    )
  return(tableau_complet)
}

#Appliquer la méthodes à 100 tableaux

set.seed(123) 
simuler_arrondi_aleatoire <- replicate(100, appliquer_arrondi_aleatoire(sample(1:100000, 1)), simplify = FALSE)

#Calcul des distances pour chaque tableau

distance_arrondi_aleatoire <- bind_rows(lapply(seq_along(simuler_arrondi_aleatoire), function(i) {
  tableau <- simuler_arrondi_aleatoire[[i]]
  res <- calcul_distance(tableau, "nb_obs", "nb_obs_alea")
  data.frame(
    MAE = res$MAE,
    HD = res$HD
  )
}))

#Moyenne des distances

distance_arrondi_aleatoire %>%
  summarize(mean(MAE),mean(HD))

#Méthode CKM

#Appliquer la méthodes à 1 tableau

appliquer_ckm <- function(seed) {
  set.seed(seed)
  
  micro_data <- tibble(
    DIPL = sample(c("CAP", "BAC", "LIC", "MAST"), N, replace = TRUE, prob = c(0.1,0.5,0.3,0.1)),
    REGION = sample(1:13, N, replace = TRUE)
  )
  
  micro_data$rkeys <- cellKey::ck_generate_rkeys(dat = micro_data, nr_digits = 5+log(N)/log(10))
  
  p_table <- ptable::create_cnt_ptable(D = D, V = V)
  
  tableau_complet <- rtauargus::tabulate_micro_data( 
    micro_data,
    cat_vars = c("DIPL","REGION"),
    resp_var = "rkeys", 
    marge_label = "Total"
  )
  
  tableau_complet[
    ,
    `:=`(
      rkeys_max = NULL, # on supprime la colonne rkeys_max pas utile pour nous
      cell_key = rkeys_tot %% 1, # on récupère la partie décimale de la somme des clés
      i = ifelse(nb_obs <= p_table@pParams@D, nb_obs, p_table@pParams@D) #par commodité pour la fusion
      #les probas de transition pour les valeurs > D sont identiques à i = D
    )
  ]
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
  
  tableau_complet <- tableau_complet %>% 
    as_tibble() %>% 
    mutate(nb_obs_pert = nb_obs + v)
  
  return(tableau_complet)
}

#Appliquer la méthodes à 100 tableaux

set.seed(123) 
simuler_ckm <- replicate(100, appliquer_ckm(sample(1:100000, 1)), simplify = FALSE)

#Calcul des distances pour chaque tableau

distance_ckm <- bind_rows(lapply(seq_along(simuler_ckm), function(i) {
  tableau <- simuler_ckm[[i]]
  res <- calcul_distance(tableau, "nb_obs", "nb_obs_pert")
  data.frame(
    MAE = res$MAE,
    HD = res$HD
  )
}))

#Moyenne des distances

distance_ckm %>%
  summarize(mean(MAE),mean(HD))
