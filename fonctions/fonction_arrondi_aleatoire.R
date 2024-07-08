#' Pose un arrond aléatoire sur un tableau
#'
#' @param tableau dataframe 
#' @param B base d'arrondi - numérique
#'
#' @return un dataframe, le tableau de départ complété des arrondis aléatoires
#' @export
#'
#' @examples
#' library(dplyr)
#' tableau_complet <- generer_tableau(100)
#' tab_avec_AL <- appliquer_arrondi_aleatoire(tableau_complet, 10)
#' str(tab_avec_AL)
appliquer_arrondi_aleatoire <- function(tableau, B = 10) {
  
  tableau <- tableau %>%
    mutate(
      p_inf = (B - (nb_obs %% B))/B,
      p_sup = 1 - p_inf,
      val_inf = floor(nb_obs/B)*B,
      val_sup = ceiling(nb_obs/B)*B
    )
  
  tableau <- tableau %>% 
    mutate(
      nb_obs_alea = apply(
        tableau %>% select(p_inf, p_sup, val_inf, val_sup),
        MARGIN = 1,
        FUN = \(r) sample(
          c(r[["val_inf"]], r[["val_sup"]]), 
          size = 1, 
          prob = c(r[["p_inf"]], r[["p_sup"]])
        )
      )
    ) %>% 
    select(-p_inf, -p_sup, -val_inf, -val_sup)
  
  return(tableau)
}
