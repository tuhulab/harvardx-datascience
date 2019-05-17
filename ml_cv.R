library(tidyverse)
library(caret)
set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

fit <- train(x_subset, y, method = "glm")
fit$results

library(devtools)
#devtools::install_bioc("genefilter")
library(genefilter)
tt <- colttests(x, y)
pvals <- tt$p.value

ind <- which(pvals <.01)
x_selected <- x[,ind]
fit <- train(x_selected , y, method="glm")
fit$results

fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)

fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(1, 27, 2)))
ggplot(fit)
