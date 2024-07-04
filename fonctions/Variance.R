VR <- function(tableau_complet, n_obs,n_obs_pert){
  obs <- tableau_complet[[n_obs]]
  obs_pert <- tableau_complet[[n_obs_pert]]
  V_orig <- mean((obs -(mean(obs)))^2 )
  V_pert <- mean((obs_pert - (mean(obs)))^2)
  vr <- 100*(abs(V_orig - V_pert)/V_orig)
  return(vr)
}

VR_contingence <- function(table_orig, table_pert) {
  obs <- as.vector(tableau_obs)
  obs_pert <- as.vector(tableau_obs_pert)
  
  V_orig <- mean((obs -(mean(obs)))^2 )
  V_pert <- mean((obs_pert - (mean(obs)))^2)
  
  vr <- 100*(abs(V_orig - V_pert)/V_orig)
  
  return(vr)
}

