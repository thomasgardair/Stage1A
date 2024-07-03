# Modélisation log-linéaire
tab_sans_marges <- tableau_complet %>% filter(REGION != "Total" & AGE != "Total" & SEX != "Total" & DIPL != "Total")

table_contingence_orig  <- xtabs(nb_obs ~ REGION + AGE, data = tab_sans_marges)
table_contingence_pert  <- xtabs(nb_obs_pert ~ REGION + AGE, data = tab_sans_marges)

# Créer les tableaux de contingence pour les données d'origine et perturbées
contingency_table_orig <- xtabs(nb_obs ~ SEX + DIPL + AGE + REGION, data = tableau_complet)
contingency_table_pert <- xtabs(nb_obs_pert ~ SEX + DIPL + AGE + REGION, data = tableau_complet)

# Ajuster les modèles log-linéaires
model_orig <- loglm(~ REGION * AGE, data = table_contingence_orig)
model_pert <- loglm(~ REGION* AGE, data = table_contingence_pert)

# Extraire la deviance (statistique de vraisemblance)
L2_orig <- model_orig$deviance
L2_pert <- model_pert$deviance

LR <- L2_pert / L2_orig

list(
  L2_orig = L2_orig,
  L2_pert = L2_pert,
  LR = LR
)