library(caret)
library(tidyverse)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

set.seed(2)
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

#which X has greatest overall accuracy
train %>% group_by(Species) %>%
  summarize(mean(Sepal.Length),mean(Sepal.Width),mean(Petal.Length),mean(Petal.Width))

### train set
sepal_len_cutoff <- seq(min(train$Sepal.Length), max(train$Sepal.Length), by = 0.1)
sepal_wid_cutoff <- seq(min(train$Sepal.Width), max(train$Sepal.Width), by = 0.1)
petal_len_cutoff <- seq(min(train$Petal.Length), max(train$Petal.Length), by = 0.1)
petal_wid_cutoff <- seq(min(train$Petal.Width), max(train$Petal.Width), by = 0.1)
sepal_len_accuracy <- map_dbl(sepal_len_cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Length > x, 'virginica', 'versicolor') %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
sepal_wid_accuracy <- map_dbl(sepal_wid_cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Width > x, 'virginica', 'versicolor') %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
petal_len_accuracy <- map_dbl(petal_len_cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x, 'virginica', 'versicolor') %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
petal_wid_accuracy <- map_dbl(petal_wid_cutoff, function(x){
  y_hat <- ifelse(train$Petal.Width > x, 'virginica', 'versicolor') %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})

max(sepal_len_accuracy)
max(sepal_wid_accuracy)
max(petal_len_accuracy)
max(petal_wid_accuracy)

###extract opt cutoff (petal_len and petal_wid)
# petal_len_opt_cutoff <- petal_len_cutoff[which.max(petal_len_accuracy)]
# petal_wid_opt_cutoff <- petal_wid_cutoff[which.max(petal_wid_accuracy)]
# 
# y_hat <- ifelse(test$Petal.Length > petal_len_opt_cutoff | test$Petal.Width> petal_wid_opt_cutoff, 
#                 'virginica', 'versicolor') %>% 
#     factor(levels = levels(test$Species))
# mean(y_hat == test$Species)


###test set
# foo <- function(x){
#   rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
#   sapply(rangedValues,function(i){
#     y_hat <- ifelse(x>i,'virginica','versicolor')
#     mean(y_hat==test$Species)
#   })
# }
# predictions <- apply(test[,3:4],2,foo)
# sapply(predictions,max)	

petal_len_cutoff <- seq(range(train$Petal.Length)[1],range(train$Petal.Length)[2],by=0.1)
petal_wid_cutoff <- seq(range(train$Petal.Width)[1],range(train$Petal.Width)[2],by=0.1)
combined_accuracy <- map_dbl(petal_wid_cutoff, function(x){
  y_hat <- ifelse((train$Petal.Width > x) OR (train$Petal.Width>x), 'virginica', 'versicolor') %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
