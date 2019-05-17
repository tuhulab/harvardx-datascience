library(tidyverse)
library(dslabs)
data("mnist_27")
mnist_27$test%>% ggplot(aes(x_1, x_2, color = y)) +  geom_point()

library(caret)
knn_fit <- knn3(y ~., data=mnist_27$train)
knn_fit <- knn3(y ~., data=mnist_27$train, k=5)

y_hat_knn <- predict(knn_fit,
                     mnist_27$test,
                     type="class")

confusionMatrix(data = y_hat_knn,
                reference= mnist_27$test$y)$overall["Accuracy"]

##linear model fit
fit_lm <- mnist_27$train %>% mutate(y=ifelse(y == 7,1,0)) %>% lm(y~x_1+x_2,data=.)
p_hat_lm <- predict(fit_lm, mnist_27$test)
y_hat_lm <- factor(ifelse(p_hat_lm>.5,7,2))
confusionMatrix(data=y_hat_lm, reference = mnist_27$test$y)$overall["Accuracy"]


##exercise_Q1
set.seed(1)
data(heights)
index <-createDataPartition(y = heights[,1],
                            times = 1,
                            p = .5,
                            list = FALSE)
train <- heights %>% slice(index)
test <- heights %>% slice(-index)


#Q1_test one
knn_fit <- knn3(sex~height,data = train, k=10)
y_hat_knn <- predict(knn_fit,
                     test,
                     type="class")
mean(y_hat_knn == test$sex)
result <- confusionMatrix(data = y_hat_knn, reference = test$sex)
result$byClass[7]

#Q1_test function
apply_knn <- function(k=...){
  set.seed(1)
  index <-createDataPartition(y = heights[,1],
                              times = 1,
                              p = .5,
                              list = FALSE)
  train <- heights %>% slice(index)
  test <- heights %>% slice(-index)
  knn_fit <- knn3(sex~height,data = train,k=k)
  y_hat_knn <- predict(knn_fit,
                       test,
                       type="class")
  mean(y_hat_knn == test$sex)
  result <- confusionMatrix(data = y_hat_knn, reference = test$sex)
  return(result$byClass[7])
}
apply_knn(k=5)

#Q1_test multiple apply
k <- seq(1,101,3)
F1s <- sapply(k,apply_knn)
max(F1s)

k[which.max(F1s)]
qplot(k,F1s)

#########################correct answer##################################
library(dslabs)
library(tidyverse)
library(caret)
data("heights")

set.seed(1)
test_index <- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]     
N
ks <- seq(1, 101, 3)
F_1 <- sapply(ks, function(k){
  fit <- knn3(sex ~ height, data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class") %>% 
    factor(levels = levels(train_set$sex))
  F_meas(data = y_hat, reference = test_set$sex)
})
plot(ks, F_1)
max(F_1)
ks[which.max(F_1)]
#################################end######################################





#Q2
library(dslabs)
data("tissue_gene_expression")
apply_knn <- function(k=...){
  set.seed(1)
  index <- createDataPartition(y = tissue_gene_expression$y,times = 1,p = .5,list = FALSE)
  train_set_x <- tissue_gene_expression$x[index,]
  train_set_y <- tissue_gene_expression$y[index]
  test_set_x <- tissue_gene_expression$x[-index,]
  test_set_y <- tissue_gene_expression$y[-index]
  knn_fit <- knn3(train_set_x,train_set_y,k=k)
  test_set_y_hat <- predict(knn_fit,
                            test_set_x,
                            type="class")
  mean(test_set_y_hat == test_set_y)}
  k=c(1,3,5,7,9,11)
  sapply(k,FUN=apply_knn)




