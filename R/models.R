
#' @import dplyr
fit_models <- function(formulas, focal_var, data) {
  models <- data_frame(
    formula = formulas,
    model = purrr::map(formulas, lme4::lmer, data = data),
    # Identify the first formula as the base model
    is_base = c(TRUE, rep(FALSE, times = length(formulas) - 1))
  )
  
  summaries <- extract_model_summaries(models) %>%
    filter(term == focal_var)

  left_join(models, summaries)
}

#' @import dplyr
extract_model_summaries <- function(models) {
  models %>%
    rowwise() %>%
    do({
      # Tidy the model, and add the model formula as the first column
      mod_summary <- tidy(.$mod, effects = "fixed")
      mod_summary$formula <- .$formula
      mod_summary %>% select(formula, everything())
    }) %>%
    mutate(
      # Assign discrete significance level
      significance_level = cut(statistic,
                               breaks = c(-Inf, 2, 3, Inf),
                               labels = c("<2", "2-3", ">3")),
      significance_level = factor(significance_level, levels = c("<2", "2-3", ">3"))
    )
}
