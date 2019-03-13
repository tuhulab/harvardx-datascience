library(HistData)
library(tidyverse)
data('GaltonFamilies')

set.seed(4881) #4881 is my lucky number.

galton_heights <- GaltonFamilies %>%
  filter(gender=='male') %>% 
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son=childHeight)

galton_heights %>% ggplot(aes(father,son)) + 
  geom_point(alpha=.5)

#Calculate correlation coefficient
galton_heights %>% summarize(r = cor(father, son)) %>% pull(r)

#Sample Correlation is a random variable
#e.g. assuming 179 pairs are population. However, we can only afford to measure 25 pairs.
galton_heights_sample <- sample_n(galton_heights, 25, replace = TRUE) %>% 
  summarize(r = cor(father, son)) %>%
  pull(r)
#run a monte carlo simulation
N=1000
n=25
cor <- replicate(N,
                 {
                   sample_n(galton_heights,n,replace = FALSE) %>%
                     summarize(r=cor(father,son)) %>%
                     pull(r)
                 })
mean(cor)
sd(cor)
qplot(cor, geom = "histogram", binwidth = 0.05, color = I("black"))


#summarize fathers and sons' height
#  galton_heights %>% 
#    summary(mean(father),sd(father),mean(son),sd(son))

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
  geom_point(alpha=.5) +
  geom_abline(intercept = 0, slope = r)

