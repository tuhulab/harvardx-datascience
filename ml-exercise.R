library(tidyverse)
library(caret)

#Galton's data
library(HistData)
galton_heights <- GaltonFamilies %>%
  filter(childNum ==1 & gender=="male") %>%
  select(father, childHeight) %>%
  rename(son=childHeight)
y <- galton_heights$son
test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)
train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice (test_index)
avg <- mean(train_set$son)
avg
mean((avg-test_set$son)^2)

fit <- lm(son~father, data=train_set)
fit$coef

y_hat <- predict(fit,test_set)
mean((y_hat-test_set$son)^2)

#exercise
set.seed(1)
rmses<- replicate(n = 100,{
  n <- 100
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))

  index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
  
   train_set <- dat %>% slice(index)
  
  test_set <- dat %>% slice(-index)
  
  fit <- lm(y~x,data=train_set)
  
  y_hat<-predict(fit,newdata = test_set)
  
  mean((y_hat-test_set$y)^2)

})
mean(rmses)
sd(rmses)


