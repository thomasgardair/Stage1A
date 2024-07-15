calculate_ratios <- function(data) {
  individual_ratios <- lapply(data, function(column) prop.table(table(column)))
  combination_ratios <- list()
  variable_names <- names(data)
  for (i in 1:(ncol(data) - 1)) {
    for (j in (i + 1):ncol(data)) {
      var1 <- variable_names[i]
      var2 <- variable_names[j]
      combination_ratios[[paste(var1, var2, sep = "_")]] <- prop.table(table(data[[var1]], data[[var2]]))
    }
  }
  
  list(
    individual_ratios = individual_ratios,
    combination_ratios = combination_ratios
  )
}
