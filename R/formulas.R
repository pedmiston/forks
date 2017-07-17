generate_formulas <- function(base_formula, covariates) {
  n_covariates <- length(covariates)
  covariate_ids <- unlist(
    lapply(1:n_covariates, function(m) combn(1:n_covariates, m, simplify = FALSE)),
    recursive = FALSE
  )
  formulas <- purrr::map(covariate_ids, function(ids) {
    paste(base_formula, paste(covariates[ids], collapse = " + "), sep = " + ")
  })
  c(base_formula, unlist(formulas))
}
