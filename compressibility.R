library(tidyverse)
library(tidygraph)
library(ggraph)
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
    model = sapply(formula, function(f) lmer(f, data = bibles))
  )

models %>%
  rowwise() %>%
  do(
    tidy(.$model, effects = "fixed")
  )
