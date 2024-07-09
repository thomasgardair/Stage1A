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