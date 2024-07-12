generer_graphique_distances <- function(distances_df) {
  # Transformer les données pour ggplot
  distances_long <- distances_df %>%
    gather(key = "Type_Distance", value = "Distance", -Tableau, -Taille)
  
  # Créer le graphique
  plot_distances <- ggplot(distances_long, aes(x = Taille, y = Distance, color = Type_Distance)) +
    geom_point() +
    labs(
      title = "Distances en fonction de la taille des tableaux",
      x = "Taille des tableaux",
      y = "Distances",
      color = "Type de Distance"
    ) +
    theme_minimal()
  
  return(plot_distances)
}
