library(tidyverse)
library(dslabs)
library(caret)
data("mnist_27")
train_glm <- train(y ~ ., method = "glm", data = mnist_27$train)
train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)
y_hat_glm <- predict(train_glm, mnist_27$test, type = "raw")
y_hat_knn <- predict(train_knn, mnist_27$test, type = "raw")
confusionMatrix(y_hat_glm, mnist_27$test$y)$overall[["Accuracy"]]
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall[["Accuracy"]]

getModelInfo("knn")
modelLookup("knn")
train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)
ggplot(train_knn, highlight = TRUE)

set.seed(2008)
train_knn <- train(y ~ ., method = "knn", 
                   data = mnist_27$train,
                   tuneGrid = data.frame(k = seq(9, 71, 2)))
ggplot(train_knn, highlight = TRUE)
train_knn$bestTune
train_knn$finalModel
confusionMatrix(predict(train_knn, mnist_27$test, type = "raw"),
                mnist_27$test$y)$overall["Accuracy"]

control <- trainControl(method = "cv", number = 10, p = .9)
train_knn_cv <- train(y ~ ., method = "knn", 
                      data = mnist_27$train,
                      tuneGrid = data.frame(k = seq(9, 71, 2)),
                      trControl = control)
ggplot(train_knn_cv, highlight = TRUE)

#Q1
library(Rborist)
n <- 1000
sigma <- 0.25
set.seed(1)
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

rftrain <- train(y ~ x, data=dat, 
                 method= "Rborist", 
                 tuneGrid= data.frame(minNode=seq(25, 100, 25),
                                      predFixed=1))
ggplot(rftrain)


dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_smooth(aes(y_hat, x), col = 2)



#Q3
data("tissue_gene_expression")
set.seed(1991)
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
rpart_train <- train(x,y, 
                     method= "rpart", 
                     tuneGrid=data.frame(cp=seq(0,0.1,0.01)))
confusionMatrix(rpart_train)
