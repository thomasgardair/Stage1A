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

table_orig <- xtabs(nb_obs ~ REGION + AGE, data = tab_sans_marges)
table_orig <- xtabs(nb_obs_pert ~ REGION + AGE, data = tab_sans_marges)


Taux_Variation_Vcramer <- function(table_orig, table_pert) {

  test_orig <- chisq.test(table_orig)
  test_pert <- chisq.test(table_pert)
  
  vcramer_diff <- "L'indépendance n'est pas rejetée pour l'un ou les deux tableaux."
  
  if (test_orig$p.value < 0.05 && test_pert$p.value < 0.05) {
    vcramer_original <- Vcramer(table_pert)
    vcramer_perturbed <- Vcramer(table_pert)
    
    vcramer_diff <- (abs(vcramer_original - vcramer_perturbed)/vcramer_original)*100
  }
  
  return(list(test_orig = test_orig, test_pert = test_pert, vcramer_diff = vcramer_diff))
}

