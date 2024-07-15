generer_graphique_distances <- function(distances_df) {
  distances_long <- distances_df %>%
    gather(key = "Type_Distance", value = "Distance", -Tableau, -Taille)

  plot_distances <- ggplot(distances_long, aes(x = Taille, y = Distance, color = Type_Distance)) +
    geom_point() +
    labs(
      title = "Distance en fonction du nombre de celllules du tableau",
      x = "Nombres de cellules",
      y = "Distance",
      color = "Type de Distance"
    ) +
    theme_minimal()
  
  return(plot_distances)
}
