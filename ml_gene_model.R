library(dslabs)
library(caret)
library(tidyverse)
library(ggplot2)
data("tissue_gene_expression")

set.seed(1993) # use this line of code if you are using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

lda_model <- train(x,y,method = "lda",preProcess = "center")
lda_model$results["Accuracy"]
cerebellum <- lda_model$finalModel$means[1,]
hippocampus <- lda_model$finalModel$means[2,]
lda_data <- data.frame(cerebellum,hippocampus) 
lda_data %>% ggplot(aes(x=cerebellum, y=hippocampus,label=rownames(data))) + geom_point() + geom_label()


set.seed(1993) # use this line of code if you are using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]
qda_model <- train(x,y,method = "qda")
qda_model$results["Accuracy"]
cerebellum <- qda_model$finalModel$means[1,]
hippocampus <- qda_model$finalModel$means[2,]
qda_data <- data.frame(cerebellum,hippocampus) 
qda_data %>% ggplot(aes(x=cerebellum, y=hippocampus,label=rownames(data))) + geom_point() + geom_label()

set.seed(1993) # use this line of code if you are using R 3.5 or earlier
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]

lda_center_model <- train(x,y,method = "lda",preProcess = "center")
lda_center_model$results["Accuracy"]
cerebellum <- lda_center_model$finalModel$means[1,]
hippocampus <- lda_center_model$finalModel$means[2,]
qda_data <- data.frame(cerebellum,hippocampus) 
qda_data %>% ggplot(aes(x=cerebellum, y=hippocampus,label=rownames(data))) + geom_point() + geom_label()
