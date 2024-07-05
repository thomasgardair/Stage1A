library(tibble)

#Paramètres fixes pour les deux méthodes

N =10000
B=10
D=10
V=6.25
seed=40889


appliquer_ckm <- function(seed) {
  set.seed(seed)
  
  micro_data <- tibble(
    SEX = sample(c("H","F"), N, replace = TRUE, prob = c(0.48,0.52)),
    DIPL = sample(
      c("CAP", "BAC", "LIC", "MAST"), N, replace = TRUE, prob = c(0.1,0.5,0.3,0.1)),
    AGE = sample(seq(0,100,10), N, replace = TRUE),
    REGION = sample(1:13, N, replace = TRUE)
  )
  micro_data <- micro_data %>%
    mutate(DIPL = if_else(AGE < 20, "<BAC", DIPL))
  
  departments <- c("a", "b", "c", "d")
  micro_data <- micro_data %>%
    mutate(DEPT = paste(REGION, sample(departments, N, replace = TRUE), sep = "_"))
  
  micro_data$rkeys <- cellKey::ck_generate_rkeys(dat = micro_data, nr_digits = 5+log(N)/log(10))
  
  p_table <- ptable::create_cnt_ptable(D = D, V = V)
  
  tableau_complet <- rtauargus::tabulate_micro_data( # fonction d'agrégation
    micro_data,
    cat_vars = c("SEX", "DIPL", "AGE", "REGION", "DEPT"),
    resp_var = "rkeys", #pour agréger les clés en même temps que de réaliser les comptages
    marge_label = "Total"
  )
  
  tableau_complet[
    ,
    `:=`(
      rkeys_max = NULL, # on supprime la colonne rkeys_max pas utile pour nous
      cell_key = rkeys_tot %% 1, # on récupère la partie décimale de la somme des clés
      i = ifelse(nb_obs <= p_table@pParams@D, nb_obs, p_table@pParams@D) #par commodité pour la fusion
      #les probas de transition pour les valeurs > D sont identiques à i = D
    )
  ]
  tableau_complet[, cell_key_end := cell_key]
  setkey(tableau_complet, i, cell_key, cell_key_end)
  
  table_transition <- p_table@pTable[, .(i,v,p_int_lb,p_int_ub)]
  setkey(table_transition, i, p_int_lb, p_int_ub)
  
  res <- foverlaps(tableau_complet, table_transition, mult = "all")
  
  if (nrow(res) == nrow(tableau_complet) & 
      nrow(res[cell_key > p_int_ub | cell_key < p_int_lb,]) == 0) {
    tableau_complet <- res
  } else {
    stop("Erreur lors de la fusion")
  }
  
  tableau_complet <- tableau_complet %>% 
    as_tibble() %>% 
    mutate(nb_obs_pert = nb_obs + v)
  
  return(tableau_complet)
}



#Appliquer la méthode à 100 tableaux

set.seed(123) 
simuler_ckm <- replicate(100, appliquer_ckm(sample(1:100000, 1)), simplify = FALSE)

afc <- function(table_contingence_orig, table_contingence_pert){
  afc_orig <- CA(table_contingence_orig, graph = FALSE)
  afc_pert <- CA(table_contingence_pert, graph = FALSE)

  coords_row <- afc_orig$row$coord[,c(1,2)] %>% as.data.frame() %>% 
    rename(x = `Dim 1`, y = `Dim 2`) %>% 
    mutate(REGION = rownames(afc_orig$row$coord), TYPE="orig") %>% 
    bind_rows(
      afc_pert$row$coord[,c(1,2)] %>% as.data.frame() %>% 
        rename(x = `Dim 1`, y = `Dim 2`) %>% 
        mutate(REGION = rownames(afc_pert$row$coord),TYPE="pert"))

  plot_row <- coords_row %>% 
    ggplot() +
    geom_point(aes(x=x, y=y, color = TYPE)) +
    ggrepel::geom_text_repel(aes(x=x, y=y, color = TYPE, label = REGION))+
    labs(
      title = "AFC ROW",
      x = "Dim 1",
      y = "Dim 2",
      color= "Type"
    )+
    theme_minimal()+
    geom_hline(yintercept = 0, color = "black", linetype="dashed") +
    geom_vline(xintercept = 0, color = "black",linetype="dashed")

  coords_col <- afc_orig$col$coord[,c(1,2)] %>% as.data.frame() %>% 
    rename(x = `Dim 1`, y = `Dim 2`) %>% 
    mutate(AGE = rownames(afc_orig$col$coord), TYPE="orig") %>% 
    bind_rows(
      afc_pert$col$coord[,c(1,2)] %>% as.data.frame() %>% 
        rename(x = `Dim 1`, y = `Dim 2`) %>% 
        mutate(AGE = rownames(afc_pert$col$coord),TYPE="pert"))

  plot_col <- coords_col %>% 
    ggplot() +
    geom_point(aes(x=x, y=y, color = TYPE)) +
    ggrepel::geom_text_repel(aes(x=x, y=y, color = TYPE, label = AGE))+
    labs(
      title = "AFC COL",
      x = "Dim 1",
      y = "Dim 2",
      color= "Type"
    )+
    theme_minimal()+
    geom_hline(yintercept = 0, color = "black", linetype="dashed") +
    geom_vline(xintercept = 0, color = "black",linetype="dashed")
  
  return(list(plot_row = plot_row, plot_col = plot_col, test_orig = test_orig, test_pert = test_pert))
}


tab_sans_marges <- tableau_complet %>% filter(REGION != "Total" & AGE != "Total" & SEX != "Total" & DIPL != "Total")

table_contingence_orig  <- xtabs(nb_obs ~ REGION + AGE, data = tab_sans_marges)#ok
table_contingence_pert  <- xtabs(nb_obs_pert ~ REGION + AGE, data = tab_sans_marges)

table_contingence_orig  <- xtabs(nb_obs ~ REGION + SEX, data = tab_sans_marges)#pb
table_contingence_pert  <- xtabs(nb_obs_pert ~ REGION + SEX, data = tab_sans_marges)

table_contingence_orig  <- xtabs(nb_obs ~ REGION + DIPL, data = tab_sans_marges)#ok
table_contingence_pert  <- xtabs(nb_obs_pert ~ REGION + DIPL, data = tab_sans_marges)

table_contingence_orig  <- xtabs(nb_obs ~ AGE + SEX, data = tab_sans_marges)#pb
table_contingence_pert  <- xtabs(nb_obs_pert ~ AGE + SEX, data = tab_sans_marges)

table_contingence_orig  <- xtabs(nb_obs ~ AGE + DIPL, data = tab_sans_marges)#ok
table_contingence_pert  <- xtabs(nb_obs_pert ~ AGE + DIPL, data = tab_sans_marges)

table_contingence_orig  <- xtabs(nb_obs ~ DIPL + SEX, data = tab_sans_marges)#pb
table_contingence_pert  <- xtabs(nb_obs_pert ~ DIPL + SEX, data = tab_sans_marges)






