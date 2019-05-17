library(tidyverse)
library(caret)
library(dslabs)
set.seed(1995)
y<-mnist_27$train$y
indexes <- createResample(mnist_27$train$y, 10)

times_3 <- function(n=...){
  sum(indexes[[n]] ==3)
}
  
n=1:10
times_3_in_10 <- sapply(n, FUN = times_3)
sum(times_3_in_10)

quantile_75<- replicate(n=10000,{
  y <- rnorm(100, 0, 1)
  quantile(y, 0.75)})
mean(quantile_75)
sd(quantile_75)

set.seed(1)
y <- rnorm(100, 0, 1)
set.seed(1)
indexes <- createResample(y, 10000)

n <- 1:10000
estimate_75_bootstrap <- function(n=...){
  quantile(y[indexes[[n]]],.75)
}
n75s <- sapply(n,estimate_75_bootstrap)
mean(n75s)
sd(n75s)
