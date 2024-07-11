calculer_ratios_categorie <- function(tableau, var_cat, var_num) {
  somme_categorie <- aggregate(tableau[[var_num]], by = list(tableau[[var_cat]]), FUN = sum)
  colnames(somme_categorie) <- c(var_cat, paste("somme", var_num, sep = "_"))
  somme_totale <- sum(tableau[[var_num]])
  somme_categorie[paste("ratio", var_num, sep = "_")] <- somme_categorie[[paste("somme", var_num, sep = "_")]] / somme_totale
  
  return(somme_categorie)
}

calculer_ratios_pour_toutes_les_categories <- function(tableau, vars_cat, vars_num) {
  liste_ratios <- list()
  
  for (var_cat in vars_cat) {
    for (var_num in vars_num) {
      ratios <- calculer_ratios_categorie(tableau, var_cat, var_num)
      ratios <- cbind(Method = var_num, ratios)
      liste_ratios <- append(liste_ratios, list(ratios))
    }
  }
  
  df_ratios <- do.call(rbind, liste_ratios)
  
  return(df_ratios)
}