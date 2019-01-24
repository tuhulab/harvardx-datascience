library(HistData)
library(tidyverse)
data('GaltonFamilies')

set.seed(1)
galton_heights <- GaltonFamilies %>%
  filter(gender=='male') %>% 
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son=childHeight)

galton_heights %>% ggplot(aes(father,son)) + 
  geom_point(alpha=.5)

#calculate slope and intercept
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father,galton_heights$son)

k <- mu_y * r / mu_x
b <- k* (-1) * mu_x + mu_y

galton_heights %>% 
  ggplot(aes(father,son)) +
  geom_point(alpha=.5) +
  geom_abline(intercept=b,slope=k)

#normalize data
galton_heights %>%
  ggplot(aes(scale(father),scale(son))) +
  geom_point() +
  geom_abline(intercept = 0, slope = r)
