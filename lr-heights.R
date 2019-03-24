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

rss <- function(beta0, beta1, data){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}

beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)




B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    mutate(father = father - mean(father)) %>%
    lm(son ~ father, data = .) %>% .$coef 
})

  
  cor(lse[1,], lse[2,]) 
  
  
galton_heights %>% ggplot(aes(father, son)) +
    geom_point() +
    geom_smooth(method = "lm")

model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as.tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))

model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model)
data <- as.tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_point(data = galton_heights, aes(x = father, y = son))
