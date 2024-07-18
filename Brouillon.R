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


all_statistiques_long <- all_statistiques %>%drop_na() %>% select(Tableau,Taille,V_Cramer_orig, V_Cramer_ckm, V_Cramer_alea ) %>%
  pivot_longer(cols = c(V_Cramer_orig, V_Cramer_ckm, V_Cramer_alea), names_to = "Type", values_to = "Value")

ggplot(all_statistiques_long, aes(x = Type, y = Value, fill = Type)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(title = "Boxplot des V_cramer par type",
       x = "Type",
       y = "Valeur V_cramer",
       fill = "Type") +
  theme_bw()

quantiles <- all_statistiques_long %>%
  group_by(Type) %>%
  summarize(quantile = seq(0, 1, 0.01), 
            value = quantile(Value, probs = seq(0, 1, 0.01)))

ggplot(quantiles, aes(x = quantile * 100, y = value, color = Type)) +
  geom_line(size = 1) +
  scale_y_log10() +
  labs(title = "Tracé des Centiles pour V_Cramer par type",
       x = "Centile",
       y = "Valeur V_Cramer",
       color = "Type") +
  theme_bw() +
  theme(panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_line(color = "grey95"))
