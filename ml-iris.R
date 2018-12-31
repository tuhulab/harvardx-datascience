library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species