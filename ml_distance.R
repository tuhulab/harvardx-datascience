library(tidyverse)
library(dslabs)

if(!exists("mnist")) mnist <- read_mnist()

set.seed(1995)
ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)
x <- mnist$train$images[ind,]
y <- mnist$train$labels[ind]

y[1:3]
x_1 <- x[1,]
x_2 <- x[2,]
x_3 <- x[3,]

sqrt(sum((x_1 - x_2)^2))

sqrt(sum((x_1 - x_3)^2))
sqrt(sum((x_2 - x_3)^2))

sqrt(crossprod(x_1,x_2))
d <- dist(x)
class(d)

as.matrix(d)[1:3,1:3]
image(as.matrix(d))

image(as.matrix(d)[order(y), order(y)])

d <- dist(t(x))
dim(as.matrix(d))
d_492 <- as.matrix(d)[492,]
image(1:28,1:28,matrix(d_492,28,28))

#exercise-Q1
data("tissue_gene_expression")
dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)

x <- tissue_gene_expression$x
d <- dist(tissue_gene_expression$x)
d_matrix <- as.matrix(d)[1:189,1:189]
d_matrix[1,2]
d_matrix[39,40]
d_matrix[73,74]
image(d_matrix)


#dimension reduction_example
set.seed(1988)
library(MASS)
n <- 100
x <- rbind(mvrnorm(n / 2, c(69, 69), matrix(c(9, 9 * 0.9, 9 * 0.92, 9 * 1), 2, 2)),
           mvrnorm(n / 2, c(55, 55), matrix(c(9, 9 * 0.9, 9 * 0.92, 9 * 1), 2, 2)))

qplot(x[,1],x[,2])
d <- dist(x)
as.matrix(d)[1,2]
as.matrix(d)[1,51]

z <- x[,1]
qplot(dist(z),dist(x)/sqrt(2))

qplot(dist(x),dist(z))
z  <- cbind((x[,2] + x[,1])/2,  x[,2] - x[,1])
