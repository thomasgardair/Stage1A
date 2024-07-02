wilcoxon_test <- function(tableau_complet, nb_obs, nb_obs_pert, paired = FALSE) {
  
  obs <- tableau_complet[[nb_obs]]
  obs_pert <- tableau_complet[[nb_obs_pert]]
  
  test <- wilcox.test(obs, obs_pert, paired = paired)
  
  return(test)
}
wilcoxon_test(tableau_complet, "nb_obs", "nb_obs_alea", paired = TRUE)

