#' @export
expand_formulas <- function(base_formula, covariates) {
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

calculate_arg_set_difference <- function(arg_set_a, arg_set_b, prefix) {
  arg_diff <- setdiff(arg_set_a, arg_set_b)
  if (length(arg_diff) == 0) return(NULL)
  paste(prefix, arg_diff)
}


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
