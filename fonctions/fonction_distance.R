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

calcul_distance(tableau_complet,"nb_obs","nb_obs_alea")

generate_1_tableau<- function(n, N, B, seed) {
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

