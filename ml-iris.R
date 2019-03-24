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

sep_len_cutoff <- seq(from=min(train$Sepal.Length),
    to=max(train$Sepal.Length),length.out=10)

sep_len_accuracy <- map_dbl(sep_len_cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Length > x, "Versicolor", "Virginica") #%>% factor(levels = levels(factor(y)))
  #mean(y_hat == y)
})
