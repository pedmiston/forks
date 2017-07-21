library(tidyverse)
library(tidygraph)
library(igraph)
library(ggraph)
library(lme4)
library(broom)

# Load this package, e.g. everything in the "R/" directory
devtools::load_all(".")

bibles <- read_csv("bibles.csv")
bibles$compression <- with(bibles, gzip1/origSize)

# Generate all model formulas with additions of various covariates
formulas <- generate_formulas("compression ~ logpopall + (logpopall|pair)",
                              c("numUniqueChars", "numUniqueWords", "origSize"))

# Fit all models from formula
models <- fit_models(formulas, focal_var = "logpopall", data = bibles)

# Create edges for all 1-variable differences between model formulas
edges <- get_deviations(formulas)

# Create an igraph::graph object from edges and nodes
graph <- graph_from_data_frame(edges, vertices = models)

# Draw the igraph::graph object with ggraph
ggraph(graph, layout = "kk") +
  geom_edge_link(aes(label = difference),
                 angle_calc = 'along',
                 label_dodge = unit(2.5, 'mm'),
                 arrow = arrow(length = unit(4, 'mm')), 
                 end_cap = circle(3, 'mm')) +
  geom_node_point(aes(size = significance_level, color = significance_level)) +
  geom_node_text(aes(filter = is_base, label = name), vjust = -1,
                 size = 5) +
  coord_cartesian(xlim = c(-1, 1.4), ylim = c(-1.5, 1.5)) +
  scale_size_manual(values = c(5, 3, 1)) +
  scale_color_manual(values = c("green", "yellow", "red"), drop = FALSE) +
  theme_graph()

ggsave("compressibility.png")

# ----------------------
# Extend to 4 covariates

formulas <- generate_formulas("compression ~ logpopall + (logpopall|pair)",
                              c("numUniqueChars", "numUniqueWords", "origSize", "Latitude"))
models <- fit_models(formulas, focal_var = "logpopall", data = bibles)
edges <- get_deviations(formulas)
graph <- graph_from_data_frame(edges, vertices = models)

# Draw the igraph::graph object with ggraph
ggraph(graph, layout = "with_sugiyama") +
  geom_edge_link(aes(label = difference, color = difference),
                 edge_width = 1.5,
                 angle_calc = 'along',
                 label_dodge = unit(2.5, 'mm'),
                 arrow = arrow(length = unit(4, 'mm')), 
                 end_cap = circle(3, 'mm')) +
  geom_node_point(aes(size = statistic)) +
  geom_node_label(aes(filter = is_base, label = name), vjust = -0.5,
                  size = 5) +
  coord_cartesian(xlim = c(0, 7), ylim = c(0.2, 6)) +
  scale_color_brewer("Set2") +
  theme_graph()

ggsave("compression-4.png", width = 12, height = 9)
