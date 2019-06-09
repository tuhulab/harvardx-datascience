library(dslabs)
library(tidyverse)
data("movielens")

movielens %>% as_tibble() %>% group_by(movieId) %>% select(rating) %>% mean()
