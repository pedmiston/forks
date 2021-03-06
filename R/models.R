#' Fit models to data for all formulas.
#' @param formulas An atomic vector of string formulas.
#' @param focal_var The model term of interest. Should be in every model.
#' @param data The data to fit the models to.
#' @export
fit_models <- function(formulas, focal_var, data) {
  models <- dplyr::data_frame(
    formula = formulas,
    model = purrr::map(formulas, lme4::lmer, data = data),
    # Identify the first formula as the base model
    is_base = c(TRUE, rep(FALSE, times = length(formulas) - 1))
  )
  
  summaries <- extract_model_summaries(models) %>%
    filter(term == focal_var)

  left_join(models, summaries)
}

#' Extract model summaries for dataframe of models.
#' @param models A dataframe with a "mod" column containing lmer models.
#' @import magrittr
#' @export
extract_model_summaries <- function(models) {
  models %>%
    dplyr::rowwise() %>%
    dplyr::do({
      # Tidy the model, and add the model formula as the first column
      mod_summary <- broom::tidy(.$mod, effects = "fixed")
      mod_summary$formula <- .$formula
      mod_summary %>% select(formula, everything())
    }) %>%
    dplyr::mutate(
      # Assign discrete significance level
      significance_level = cut(statistic,
                               breaks = c(-Inf, 2, 3, Inf),
                               labels = c("<2", "2-3", ">3")),
      significance_level = factor(significance_level, levels = c("<2", "2-3", ">3"))
    )
}
