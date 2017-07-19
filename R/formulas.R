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

compare_formulas <- function(from, to) {
  from_args <- split_formula_args(from)
  to_args <- split_formula_args(to)
  
  arg_diff <- setdiff(to_args, from_args)
  
  data_frame(
    from = from,
    to = to,
    difference = arg_diff,
    n_different = length(arg_diff)
  )
}

split_formula_args <- function(formula_string) {
  formula_args <- as.character(as.formula(formula_string))
  predictors <- formula_args[3]
  strsplit(gsub("[[:blank:]]", "", predictors), "\\+")[[1]]
}


get_deviations <- function(formulas) {
  all_pairwise <- tibble::as_data_frame(t(combn(formulas, m = 2)))
  names(all_pairwise) <- c("from", "to")
  all_pairwise %>%
    rowwise() %>%
    do(compare_formulas(.$from, .$to)) %>%
    ungroup() %>%
    filter(n_different == 1)
}