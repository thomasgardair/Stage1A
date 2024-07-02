Vcramer <- function(tab) {
  if (!is.table(tab)) {
    stop("L'argument doit être une table de contingence.")
  }
  chi2 <- chisq.test(tab)$statistic
  n <- sum(tab)  
  k <- min(dim(tab)) - 1
  vcramer <- sqrt(chi2 / (n * k))
  return(as.numeric(vcramer))
}

tab_sans_marges <- tableau_complet %>% filter(REGION != "Total" & AGE != "Total" & SEX != "Total" & DIPL != "Total")

original_tab <- xtabs(nb_obs ~ REGION + AGE, data = tab_sans_marges)
perturbed_tab <- xtabs(nb_obs_pert ~ REGION + AGE, data = tab_sans_marges)


Taux_Variation_Vcramer <- function(original_tab, perturbed_tab) {

  vcramer_original <- Vcramer(original_tab)
  vcramer_perturbed <- Vcramer(perturbed_tab)
  
  vcramer_diff <- (abs(vcramer_original - vcramer_perturbed)/vcramer_original)*100
  
  return(vcramer_diff)
}

