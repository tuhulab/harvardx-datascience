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


sep_len_cutoff <- seq(from=min(train$Sepal.Length),
    to=max(train$Sepal.Length),by = 0.1)

sep_len_compare <- function(value=...){
  ifelse(value>sep_len_cutoff,'virginica','versicolor')
}
sapply(train$Sepal.Length,sep_len_compare)

sep_len_accuracy <- map_dbl(sep_len_cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Length > x, "Versicolor", "Virginica") %>% 
    factor(levels = levels(factor(y)))
  mean(y_hat == y)
})
