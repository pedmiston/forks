library(tidyverse)
library(tidygraph)
library(ggraph)
library(igraph)
library(lme4)
library(broom)

bibles <- read_csv("bibles.csv")
bibles$compression <- with(bibles, gzip1/origSize)

models <- data_frame(
    formula = paste0("compression ~ ", c(
      "logpopall + numUniqueChars + numUniqueWords + origSize + (logpopall|pair)",
      "logpopall +                  numUniqueWords + origSize + (logpopall|pair)",
      "logpopall + numUniqueChars +                  origSize + (logpopall|pair)",
      "logpopall + numUniqueChars + numUniqueWords +            (logpopall|pair)"
    )),
    difference = c("", "— numUniqueChars", "— numUniqueWords", "— origSize"),
    model = sapply(formula, function(f) lmer(f, data = bibles))
  ) %>%
  mutate(n = 1:n()) %>%
  select(n, everything()) %>%
  mutate(formula_labeled = ifelse(n == 1, formula, difference))

mod_summaries <- models %>%
  group_by(formula) %>%
  do(
    tidy(.$model[[1]], effects = "fixed")
  ) %>%
  ungroup() %>%
  filter(term == "logpopall")

models <- left_join(models, mod_summaries)

edges <- data_frame(
  from = 1,
  to = c(2, 3, 4),
  difference = c("- numUniqueChars", "- numUniqueWords", "- origSize")
)

graph <- graph_from_data_frame(edges, vertices = models)

ggraph(graph, layout = "dendrogram") +
  geom_edge_diagonal() +
  geom_node_label(aes(label = formula_labeled, color = statistic), size = 2) +
  theme_graph() +
  coord_cartesian(xlim = c(0, 4), ylim = c(0, 1)) +
  scale_color_gradient2()
