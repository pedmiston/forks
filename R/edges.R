
#' Generate all pairs of formulas that differ in a single term.
#' @import magrittr
#' @export
expand_formula_edges <- function(formulas) {
  all_pairwise <- tibble::as_data_frame(t(combn(formulas, m = 2)))
  names(all_pairwise) <- c("from", "to")
  # Return only those edges where the formulas differ in only 1 term
  all_pairwise %>%
    dplyr::rowwise() %>%
    dplyr::do(compare_formulas(.$from, .$to)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(n_different == 1)
}

#' Generate pairs of formulas to create a tree.
#' @import magrittr
#' @export
walk_formula_tree <- function(formulas) {
  all_pairwise_edges <- expand_formula_edges(formulas)
  formulas %>%
    purrr::map(function(target) {
      eligible_edges <- dplyr::filter(all_pairwise_edges, to == target)
      if (nrow(eligible_edges) == 0) return(eligible_edges)
      dplyr::sample_n(eligible_edges, size = 1)
    }) %>%
    dplyr::bind_rows()
}
