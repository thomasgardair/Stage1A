#Fonction pour calculer les tests entre les valeurs originales et perturbées

spearman_test <- function(tableau_complet, nb_obs, nb_obs_pert) {
  
  obs <- tableau_complet[[nb_obs]]
  obs_pert <- tableau_complet[[nb_obs_pert]]
  
  test <- cor.test(obs,obs_pert, method = "spearman")
  
  return(test)
}
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

#Calcul des tests pour chaque tableau

Spearman_ckm <- bind_rows(lapply(seq_along(simuler_ckm), function(i) {
  tableau <- simuler_ckm[[i]]
  res <- spearman_test(tableau, "nb_obs", "nb_obs_pert")
  data.frame(
    S = res$statistic,
    rho = res$estimate,
    p_value = res$p.value
  )
}))

#Moyenne des stats

Spearman_ckm %>%
  summarize(mean(S),mean(rho),mean(p_value))

