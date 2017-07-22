# `forks`: Visualizing the garden of forking paths

`forks` is an R package with tools for fitting a number of related models, and
comparing their results. The garden of forking paths is realized as a graph of
nodes and edges, where nodes are models and edges connect the models via some
difference in formula.

```R
library(forks)
formulas <- expand_base_formula("y ~ x", covariates = c("a", "b"))
models <- fit_models(formulas, focal_var = "x", data = mydata)
edges <- walk_formula_tree(formulas)

# Create the graph
library(ggraph)
graph <- igraph::graph_from_data_frame(edges, vertices = edges)
ggraph(graph, layout = "sugiyama") +
  geom_edge_link(aes(label = difference), 
                 angle_calc = 'along',
                 label_dodge = unit(2.5, 'mm'),
                 arrow = arrow(length = unit(4, 'mm')), 
                 end_cap = circle(3, 'mm')) +
  geom_node_point(aes(size = estimate)) +
  geom_node_text(aes(filter = is_base, label = name),
                 vjust = -1, size = 5) +
  scale_color_brewer("Set2") +
  theme_graph()
```