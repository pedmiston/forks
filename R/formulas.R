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

#' @import magrittr
compare_formulas <- function(from, to) {
  from_args <- split_formula_args(from)
  to_args <- split_formula_args(to)
  
  positive_arg_diff <- calculate_arg_set_difference(to_args, from_args, "+")
  negative_arg_diff <- calculate_arg_set_difference(from_args, to_args, "-")
  arg_diff <- c(negative_arg_diff, positive_arg_diff)
  
  data_frame(
    from = from,
    to = to,
    difference = paste(arg_diff, collapse = " "),
    n_different = length(arg_diff)
  )
}

calculate_arg_set_difference <- function(arg_set_a, arg_set_b, prefix) {
  arg_diff <- setdiff(arg_set_a, arg_set_b)
  if (length(arg_diff) == 0) return(NULL)
  paste(prefix, arg_diff)
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