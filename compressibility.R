library(tidyverse)
library(tidygraph)
library(ggraph)
library(lme4)
library(broom)

devtools::load_all(".")

bibles <- read_csv("bibles.csv")
bibles$compression <- with(bibles, gzip1/origSize)

formulas <- generate_formulas("compression ~ logpopall + (logpopall|pair)",
                              c("numUniqueChars", "numUniqueWords", "origSize"))

models <- data_frame(
  model_id = 1:length(formulas),
  formula = formulas,
  model = map(formulas, lmer, data = bibles)
)

# edges <- get_deviations(formulas)
