models <- c("glm", "lda",  "naive_bayes",  "svmLinear", 
            "gamboost",  "gamLoess", "qda", 
            "knn", "kknn", "loclda", "gam",
            "rf", "ranger",  "wsrf", "Rborist", 
            "avNNet", "mlp", "monmlp",
            "adaboost", "gbm",
            "svmRadial", "svmRadialCost", "svmRadialSigma") #Rborist is faster

library(caret)
library(dslabs)
set.seed(1)
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models

n <- 1:length(fits)

predicts <- as.matrix(sapply(n, predict_fun <- function(n){
  predict(fits[n],mnist_27$test)}
))

pred <- sapply(fits, function(object) 
  predict(object, newdata = mnist_27$test))
dim(pred)


mean (pred == mnist_27$test$y)

#Q3
accuracy_column <- colMeans(pred==mnist_27$test$y)
accuracy_column
mean(accuracy_column)
#ensemble
n <- 1:nrow(pred)
pred_ensemble <- sapply(n,function(n){
                            ifelse(mean(pred[n,]=="2")>.5,
                                   "2",
                                   "7")}
                        )
accuracy_ensemble <- mean(pred_ensemble == mnist_27$test$y)
    

sum(accuracy_column > accuracy_ensemble)
method <- accuracy_column > accuracy_ensemble
models[method]
