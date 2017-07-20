library(tidyverse)
library(tidygraph)
library(igraph)
library(ggraph)
library(lme4)
library(broom)

devtools::load_all(".")

bibles <- read_csv("bibles.csv")
bibles$compression <- with(bibles, gzip1/origSize)

formulas <- generate_formulas("compression ~ logpopall + (logpopall|pair)",
                              c("numUniqueChars", "numUniqueWords", "origSize"))

models <- data_frame(
  formula = formulas,
  model = map(formulas, lmer, data = bibles),
  is_base = c(TRUE, rep(FALSE, times = length(formulas) - 1))
)

model_summaries <- models %>%
  rowwise() %>%
  do({
    mod_summary <- tidy(.$mod, effects = "fixed")
    mod_summary$formula <- .$formula
    mod_summary %>% select(formula, everything())
  }) %>%
  filter(term == "logpopall")

models <- left_join(models, model_summaries)

edges <- get_deviations(formulas)

graph <- graph_from_data_frame(edges, vertices = models)

ggraph(graph, layout = "kk") +
  geom_edge_link(aes(label = difference),
                 angle_calc = 'along',
                 label_dodge = unit(2.5, 'mm'),
                 arrow = arrow(length = unit(4, 'mm')), 
                 end_cap = circle(3, 'mm')) +
  geom_node_point() +
  geom_node_label(aes(filter = is_base, label = name), vjust = -0.5,
                  size = 5) +
  coord_cartesian(xlim = c(-1, 1.4), ylim = c(-1.5, 1.5))

ggsave("compressibility.png")