#' Title
#'
#' @param N nb individus - numeric
#'
#' @return tableau de données complet sous forme de dtaframe/datatable
#' @export
#'
#' @examples
#' library(dplyr)
#' tableau_complet <- generer_tableau(100)
#' str(tableau_complet)
generer_tableau <- function(N = 10000L){
  
  micro_data <- tibble(
    SEX = sample(c("H", "F"), N, replace = TRUE, prob = c(0.48, 0.52)),
    DIPL = sample(c("CAP", "BAC", "LIC", "MAST"), N, replace = TRUE, prob = c(0.1, 0.5, 0.3, 0.1)),
    AGE = sample(seq(0, 100, 10), N, replace = TRUE),
    REGION = sample(1:13, N, replace = TRUE)
  )
  
  micro_data <- micro_data %>%
    mutate(DIPL = ifelse(AGE < 20, "<BAC", DIPL))
  
  micro_data$rkeys <- cellKey::ck_generate_rkeys(dat = micro_data, nr_digits = 5+log(N)/log(10))
  
  departments <- c("a", "b", "c", "d")
  micro_data <- micro_data %>%
    mutate(DEPT = paste(REGION, sample(departments, N, replace = TRUE), sep = "_"))
  
  tableau_complet <- rtauargus::tabulate_micro_data(
    micro_data,
    cat_vars = c("SEX", "DIPL", "AGE", "REGION","DEPT"),
    resp_var = "rkeys",
    marge_label = "Total"
  ) %>% 
    select(-rkeys_max)
  
  return(tableau_complet)
}
