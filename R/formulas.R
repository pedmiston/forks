#' Expand a base formula with all combinations of covariates.
#' @param base_formula A string representation of a formula, e.g., "y ~ x".
#' @param covariates An atomic vector of covariates to add to the base formula.
#' @export
expand_base_formula <- function(base_formula, covariates) {
  n_covariates <- length(covariates)
  covariate_ids <- unlist(
    lapply(1:n_covariates, function(m) combn(1:n_covariates, m, simplify = FALSE)),
    recursive = FALSE
  )
  formulas <- purrr::map(covariate_ids, function(ids) {
    paste(covariates[ids], collapse = " + ") %>%
      paste(base_formula, ., sep = " + ")
  })
  c(base_formula, unlist(formulas))
}

#' Compare two formulas and return the differences between them.
#' @param from A string formula.
#' @param to A string formula.
#' @import magrittr
#' @export
compare_formulas <- function(from, to) {
  from_args <- split_formula_args(from)
  to_args <- split_formula_args(to)
  
  positive_arg_diff <- calculate_arg_set_difference(to_args, from_args, "+")
  negative_arg_diff <- calculate_arg_set_difference(from_args, to_args, "-")
  arg_diff <- c(negative_arg_diff, positive_arg_diff)
  
  tibble::data_frame(
    from = from,
    to = to,
    difference = paste(arg_diff, collapse = " "),
    n_different = length(arg_diff)
  )
}

#' Extract the predictors from formula strings.
#' @param formula_string A string formula.
#' @import magrittr
#' @export
split_formula_args <- function(formula_string) {
  formula_args <- formula_string %>%
    purrr::map(~ as.character(as.formula(.x))) %>%
    purrr::map(~ .x[3]) %>%
    purrr::map(~ strsplit(gsub("[[:blank:]]", "", .x), "\\+")[[1]])
  
  if (length(formula_args) == 1) return(formula_args[[1]])
  formula_args
}

calculate_arg_set_difference <- function(arg_set_a, arg_set_b, prefix) {
  arg_diff <- setdiff(arg_set_a, arg_set_b)
  if (length(arg_diff) == 0) return(NULL)
  paste(prefix, arg_diff)
}
