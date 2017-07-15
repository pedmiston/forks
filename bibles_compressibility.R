library(tidyverse)
library(tidygraph)
library(ggraph)
library(lme4)
library(broom)

bibles <- read_csv("bibles_compressibility.csv")
bibles$compression <- with(bibles, gzip1/origSize)

basic <- lmer(compression ~ logpopall +
                numUniqueChars + numUniqueWords + origSize +
                (logpopall|pair),
              data = bibles)

tidy(basic, effects = "fixed")