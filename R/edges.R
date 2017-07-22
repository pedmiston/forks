get_deviations <- function(formulas) {
  all_pairwise <- tibble::as_data_frame(t(combn(formulas, m = 2)))
  names(all_pairwise) <- c("from", "to")
  all_pairwise %>%
    rowwise() %>%
    do(compare_formulas(.$from, .$to)) %>%
    ungroup() %>%
    filter(n_different == 1)
}

get_tree_edges <- function(formulas, ordered_covariates, seed = NULL) {
  
  # !!! Assumes formulas are ordered !!!
  base_formula <- formulas[1]
  base_level <- length(split_formula_args(base_formula))
  max_formula <- formulas[length(formulas)]
  
  if (missing(ordered_covariates)) {
    # Extract covariate order from variables in max but not in base
    ordered_covariates <- setdiff(split_formula_args(max_formula),
                                  split_formula_args(base_formula))
  }
  
  # Start with all N == 1 edges
  all_pairwise <- get_deviations(formulas) %>%
    # Calculate the level of the "to" model for each from -> to edge
    # !!! Stinks !!!
    mutate(level = map(split_formula_args(.$to), ~ length(.x) - base_level) %>% unlist())
  
  level_choices <- data_frame(
    level = 0:length(ordered_covariates),
    num_branches = rev(level)
  )
  
  # Sample num_branches at each level
  set.seed(seed)
  random_edges <- level_choices %>%
    rowwise() %>%
    do({
      level <- .$level
      num_branches <- .$num_branches
      all_pairwise %>%
        filter(level == level) %>%
        sample_n(size = num_branches)
    })
  
  random_edges
}
