# Installation des packages -------------------------------------------

# install.packages("remotes")
# remotes::install_github(
#   "InseeFrLab/rtauargus",
#   build_vignettes = FALSE,
#   upgrade = "never"
# )
# 
# install.packages("ptable")
# install.packages("cellKey")
install.packages("FactoMineR")
install.packages("factoextra")

# Chargement des packages --------------------------------------------
library(dplyr)
library(data.table)
library(ggplot2)
library(ptable)
library(cellKey)
library(rtauargus)
library(FactoMineR)
library(factoextra)
library(cluster)



# Paramètres ------------------------------------------
N = 100000 # Nombre d'individus dans la table
D = 20 # Déviation de la CKM
V = 6.25 # Variance de la CKM

seed = 40889 # graine aléatoire pour reproduire le jeu

# 0-a Créer/Importer la table de données individuelles -------------------

micro_data <- tibble(
  SEX = sample(c("H","F"), N, replace = TRUE, prob = c(0.48,0.52)),
  DIPL = sample(
    c("CAP", "BAC", "LIC", "MAST"), N, replace = TRUE, prob = c(0.1,0.5,0.3,0.1)),
  AGE = sample(seq(0,100,10), N, replace = TRUE),
  REGION = sample(1:13, N, replace = TRUE)
)

str(micro_data)
summary(micro_data)

# 0-b Tirage des clés aléatoires individuelles -----------------------------
set.seed(seed)
micro_data$rkeys <- cellKey::ck_generate_rkeys(dat = micro_data, nr_digits = 5+log(N)/log(10))

# On peut vérifier que la distribution empirique des clés suit une loi uniforme
hist(micro_data$rkeys, breaks = 20)

# 0-a La matrice de transition -------------------------------------------

p_table <- ptable::create_cnt_ptable(D = D, V = V)
p_table@empResults[-1,]
plot(p_table, type = "p")
plot(p_table, type = "d")

str(p_table@pTable)



# 1-a Construire un tableau ----------------------------------------------
# On construit ici le tableau croisant toutes les variables 
# Avec toutes les marges 
# Ainsi on pourra considérer des sous-tableaux selon les analyses qu'on veut faire
tableau_complet <- rtauargus::tabulate_micro_data( # fonction d'agrégation
  micro_data,
  cat_vars = c("SEX", "DIPL", "AGE", "REGION"),
  resp_var = "rkeys", #pour agréger les clés en même temps que de réaliser les comptages
  marge_label = "Total"
)

str(tableau_complet)

# RQ: le tableau généré est de classe "data.table" 
#classe qu'on conserve le temps de faire la fusion

# 1-b On prépare la table à la fusion --------------------------------
tableau_complet[
  ,
  `:=`(
    rkeys_max = NULL, # on supprime la colonne rkeys_max pas utile pour nous
    cell_key = rkeys_tot %% 1, # on récupère la partie décimale de la somme des clés
    i = ifelse(nb_obs <= p_table@pParams@D, nb_obs, p_table@pParams@D) #par commodité pour la fusion
    #les probas de transition pour les valeurs > D sont identiques à i = D
  )
]

# Vérification de l'uniformité empirique de cell_key (clés des cellules) --
summary(tableau_complet$cell_key)
hist(tableau_complet$cell_key)

# 2- associer la perturbation pour chaque cellule --------------------------

tableau_complet[,cell_key_end := cell_key]
setkey(tableau_complet, i, cell_key, cell_key_end)

table_transition <- p_table@pTable[,.(i,v,p_int_lb,p_int_ub)]
setkey(table_transition, i, p_int_lb, p_int_ub)

res <- foverlaps(
  tableau_complet,
  table_transition,
  mult = "all"
)

if(nrow(res) == nrow(tableau_complet) & 
   nrow(res[cell_key > p_int_ub | cell_key < p_int_lb,]) == 0){
  tableau_complet <- res
}else{
  print("Erreur lors de la fusion")
}

str(tableau_complet)

tableau_complet

# Vérification des probas empiriques de transition
p_emp <- merge(
  tableau_complet[, .N, by = .(i,v)][, `:=`(p_emp = N/sum(N)), by = .(i)],
  p_table@pTable[, .(i,v,p)],
  by = c("i","v")
)
p_emp[, p_diff := abs(p - p_emp)][]
p_emp[order(p_diff, decreasing = TRUE),]
p_emp[, quantile(p_diff, seq(0,1,0.1))]


ggplot(p_emp) +
  geom_line(aes(x = v, y = p), color = "darkred") +
  geom_line(aes(x = v, y = p_emp), color = "steelblue")

# 3- Pertubation = Calcul la valeur finale -----------------------------------

tableau_complet <- tableau_complet %>% 
  as_tibble() %>% 
  mutate(nb_obs_pert = nb_obs + v)

str(tableau_complet)

#Selection des varaibles
# 
# tableau_original<-tableau_complet%>%select(AGE,REGION,nb_obs)
# tableau_pert<-tableau_complet%>%select(AGE,REGION,nb_obs_pert)

tab_sans_marges <- tableau_complet %>% filter(REGION != "Total" & AGE != "Total" & SEX != "Total" & DIPL != "Total")

table_contingence_orig  <- xtabs(nb_obs ~ REGION + AGE, data = tab_sans_marges)
table_contingence_pert  <- xtabs(nb_obs_pert ~ REGION + AGE, data = tab_sans_marges)


#Creation tableaux de contingences
# 
# table_contingence_orig <- xtabs(nb_obs ~ REGION + AGE, data = tableau_original)
# table_contingence_pert <- xtabs(nb_obs_pert ~ REGION + AGE, data = tableau_pert)
print(table_contingence_orig)
print(table_contingence_pert)

chisq.test(table_contingence_orig)
chisq.test(table_contingence_pert)

# Réalisation de des AFC

afc_orig <- CA(table_contingence_orig, graph = FALSE)
afc_pert <- CA(table_contingence_pert, graph = FALSE)

summary(afc_orig, nb.dec = 3)
summary(afc_pert, nb.dec = 3)

#Frequences marginales

par(las = 1, mar=c(4,20,4,1))
barplot(afc_orig$call$marge.col, horiz = TRUE, cex.names = 0.7)

par(las = 1, mar=c(4,20,4,1))
barplot(afc_pert$call$marge.col, horiz = TRUE, cex.names = 0.7)


barplot(afc_orig$call$marge.row[order(afc_orig$call$marge.row)],
        horiz = TRUE, cex.names = 0.7)

barplot(afc_pert$call$marge.row[order(afc_pert$call$marge.row)],
        horiz = TRUE, cex.names = 0.7)

#Info inertie 

par(las = 0, mar=c(4,4,4,1))
barplot(afc_orig$eig[,2],main = "Valeurs propres", names.arg = 1:nrow(afc_orig$eig))

par(las = 0, mar=c(4,4,4,1))
barplot(afc_pert$eig[,2],main = "Valeurs propres", names.arg = 1:nrow(afc_pert$eig))

#Contribution age 

inertia.col.orig <- round(afc_orig$col$inertia / sum(afc_orig$col$inertia), 5)
names(inertia.col.orig) <- colnames(table_contingence_orig)
inertia.col.orig

inertia.col.pert <- round(afc_pert$col$inertia / sum(afc_pert$col$inertia), 5)
names(inertia.col.pert) <- colnames(table_contingence_pert)
inertia.col.pert

tab_orig <- data.frame(poids = afc_orig$call$marge.col,
                       distance = sqrt(afc_orig$col$inertia/afc_orig$call$marge.col),
                       inertie = afc_orig$col$inertia,
                       percent.inertie = afc_orig$col$inertia/sum(afc_orig$col$inertia))
round(tab_orig, 4)

tab_pert <- data.frame(poids = afc_pert$call$marge.col,
                       distance = sqrt(afc_pert$col$inertia/afc_pert$call$marge.col),
                       inertie = afc_pert$col$inertia,
                       percent.inertie = afc_pert$col$inertia/sum(afc_pert$col$inertia))
round(tab_pert, 4)


#Contribution region

inertia.row.orig <- afc_orig$row$inertia / sum(afc_orig$row$inertia)
names(inertia.row.orig) <- rownames(table_contingence_orig)
head(inertia.row.orig[order(inertia.row.orig, decreasing = T)])


inertia.row.pert <- afc_pert$row$inertia / sum(afc_pert$row$inertia)
names(inertia.row.pert) <- rownames(table_contingence_pert)
head(inertia.row.pert[order(inertia.row.pert, decreasing = T)])

#Visualisation

# par(mfrow = c(1,2))
plot.CA(afc_orig, axes = 1:2, invisible = c("row", "row.sup"))
plot.CA(afc_pert, axes = 1:2, invisible = c("row", "row.sup"))

#Visualtion avec modalité des deux variables

plot.CA(afc_orig, axes = 1:2, invisible = c("row.sup"),
        selectRow = order(inertia.row.orig, decreasing = T))

plot.CA(afc_pert, axes = 1:2, invisible = c("row.sup"),
        selectRow = order(inertia.row.pert, decreasing = T))

round(head(data.frame(CTR=afc_orig$row$contrib[,1],
                      CO2=afc_orig$row$cos2[,1],
                      Eff=afc_orig$call$marge.row)[rev(order(afc_orig$row$contrib[,1])),]), 2)

round(head(data.frame(CTR=afc_pert$row$contrib[,1],
                      CO2=afc_pert$row$cos2[,1],
                      Eff=afc_pert$call$marge.row)[rev(order(afc_pert$row$contrib[,1])),]), 2)

round(head(data.frame(CTR=afc_orig$col$contrib[,1],
                      CO2=afc_orig$col$cos2[,1],
                      Eff=afc_orig$call$marge.col)[rev(order(afc_orig$col$contrib[,1])),]), 2)

round(head(data.frame(CTR=afc_pert$col$contrib[,1],
                      CO2=afc_pert$col$cos2[,1],
                      Eff=afc_pert$call$marge.col)[rev(order(afc_pert$col$contrib[,1])),]), 2)

#Axe 2:3

round(data.frame(CTR=afc_orig$col$contrib[,2:3],
                 CO2=afc_orig$col$cos2[,2:3]), 2)


round(data.frame(CTR=afc_pert$col$contrib[,2:3],
                 CO2=afc_pert$col$cos2[,2:3]), 2)

#liste les mieux représentés

round(head(data.frame(
  CTR=afc_orig$row$contrib[,2],
  CO2=afc_orig$row$cos2[,2],
  Eff=afc_orig$call$marge.row)[rev(order(afc_orig$row$contrib[,2])),]), 2)

round(head(data.frame(
  CTR=afc_pert$row$contrib[,2],
  CO2=afc_pert$row$cos2[,2],
  Eff=afc_pert$call$marge.row)[rev(order(afc_pert$row$contrib[,2])),]), 2)

#les regions les plus représentes ne sont pas danbs le mm ordre

round(head(data.frame(
  CTR=afc_orig$row$contrib[,3],
  CO2=afc_orig$row$cos2[,3],
  Eff=afc_orig$call$marge.row)[rev(order(afc_orig$row$contrib[,2])),]),3)

round(head(data.frame(
  CTR=afc_pert$row$contrib[,3],
  CO2=afc_pert$row$cos2[,3],
  Eff=afc_pert$call$marge.row)[rev(order(afc_pert$row$contrib[,2])),]),3)

#Representation age plan factoriel 2-3

plot.CA(afc_orig, axes = 2:3, invisible = c("row", "row.sup"))

plot.CA(afc_pert, axes = 2:3, invisible = c("row", "row.sup"))

plot.CA(afc_orig, axes = 2:3, invisible = c("row.sup"),
        selectRow="cos2 0.9")

plot.CA(afc_pert, axes = 2:3, invisible = c("row.sup"),
        selectRow="cos2 0.9")

plot.CA(afc_orig, axes = 2:3, selectRow="cos2 0.9", cex = 0.8)

plot.CA(afc_pert, axes = 2:3, selectRow="cos2 0.9", cex = 0.8)

#Clustering
# Calculer la distance entre les points des coordonnées de l'AFC
dist_orig <- dist(afc_orig$row$coord)
dist_pert <- dist(afc_pert$row$coord)

distance_eucl_region <- afc_orig$row$coord[,c(1,2)] %>% as.data.frame() %>% 
  rename(x_orig = `Dim 1`, y_orig = `Dim 2`) %>% 
  mutate(REGION = rownames(afc_orig$row$coord)) %>% 
  left_join(
    afc_pert$row$coord[,c(1,2)] %>% as.data.frame() %>% 
      rename(x_pert = `Dim 1`, y_pert = `Dim 2`) %>% 
      mutate(REGION = rownames(afc_pert$row$coord)), by="REGION") %>%
  mutate(dist_eucl=sqrt((x_orig - x_pert)^2 + (y_orig - y_pert)^2))%>%
  summarise(mean(dist_eucl))

distance_eucl_region

coords_region <- afc_orig$row$coord[,c(1,2)] %>% as.data.frame() %>% 
  rename(x = `Dim 1`, y = `Dim 2`) %>% 
  mutate(REGION = rownames(afc_orig$row$coord), TYPE="orig") %>% 
  bind_rows(
    afc_pert$row$coord[,c(1,2)] %>% as.data.frame() %>% 
      rename(x = `Dim 1`, y = `Dim 2`) %>% 
      mutate(REGION = rownames(afc_pert$row$coord),TYPE="pert"))


coords_region %>% 
  ggplot() +
  geom_point(aes(x=x, y=y, color = TYPE)) +
  ggrepel::geom_text_repel(aes(x=x, y=y, color = TYPE, label = REGION))+
  labs(
    title = "Coordonnées des régions",
    x = "Coordonnées des régions sur la Dim 1",
    y = " Coordonées des régions sur la Dim 2",
    color= "Type"
  )+
  theme_minimal()+
  geom_hline(yintercept = 0, color = "black", linetype="dashed") +
  geom_vline(xintercept = 0, color = "black",linetype="dashed")



distance_eucl_age <- afc_orig$col$coord[,c(1,2)] %>% as.data.frame() %>% 
  rename(x_orig = `Dim 1`, y_orig = `Dim 2`) %>% 
  mutate(AGE = rownames(afc_orig$col$coord)) %>% 
  left_join(
    afc_pert$col$coord[,c(1,2)] %>% as.data.frame() %>% 
      rename(x_pert = `Dim 1`, y_pert = `Dim 2`) %>% 
      mutate(AGE = rownames(afc_pert$col$coord)), by="AGE") %>%
  mutate(dist_eucl=sqrt((x_orig - x_pert)^2 + (y_orig - y_pert)^2))%>%
  summarise(mean(dist_eucl))

distance_eucl_age

coords_age <- afc_orig$col$coord[,c(1,2)] %>% as.data.frame() %>% 
  rename(x = `Dim 1`, y = `Dim 2`) %>% 
  mutate(AGE = rownames(afc_orig$col$coord), TYPE="orig") %>% 
  bind_rows(
    afc_pert$col$coord[,c(1,2)] %>% as.data.frame() %>% 
      rename(x = `Dim 1`, y = `Dim 2`) %>% 
      mutate(AGE = rownames(afc_pert$col$coord),TYPE="pert"))


coords_age %>% 
  ggplot() +
  geom_point(aes(x=x, y=y, color = TYPE)) +
  ggrepel::geom_text_repel(aes(x=x, y=y, color = TYPE, label = AGE))+
  labs(
    title = "Coordonnées des ages",
    x = "Coordonnées des ages sur la Dim 1",
    y = " Coordonées des ages sur la Dim 2",
    color= "Type"
  )+
  theme_minimal()+
  geom_hline(yintercept = 0, color = "black", linetype="dashed") +
  geom_vline(xintercept = 0, color = "black",linetype="dashed")


# Inspecter les noms des colonnes pour afc_orig$col$coord
colnames(afc_orig$col$coord)

# Inspecter les noms des colonnes pour afc_pert$col$coord
colnames(afc_pert$col$coord)



# Appliquer la CAH avec la méthode de Ward
cah_orig <- hclust(dist_orig, method = "ward.D2")
cah_pert <- hclust(dist_pert, method = "ward.D2")

# Visualiser les dendrogrammes
fviz_dend(cah_orig, k = 3, rect = TRUE, main = "Dendrogramme - Données Originales")
fviz_dend(cah_pert, k = 3, rect = TRUE, main = "Dendrogramme - Données Perturbées")

# Calculer la distance entre les points des coordonnées de l'AFC
dist_orig <- dist(afc_orig$row$coord)
dist_pert <- dist(afc_pert$row$coord)

# Appliquer la CAH avec la méthode de Ward
cah_orig <- hclust(dist_orig, method = "ward.D2")
cah_pert <- hclust(dist_pert, method = "ward.D2")



