library('Lahman')
library(tidyverse)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR/G, R_per_game=R/G) %>%
  ggplot(aes(R_per_game,HR_per_game)) + geom_point(alpha=0.5)
