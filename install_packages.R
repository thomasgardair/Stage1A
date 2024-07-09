# Installation des packages -------------------------------------------

install.packages("remotes")
remotes::install_github(
  "InseeFrLab/rtauargus",
  build_vignettes = FALSE,
  upgrade = "never"
)

install.packages("ptable")
install.packages("cellKey")
install.packages("MASS")
install.packages("dplyr")
install.packages("FactoMineR")
