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

lda_model <- train(x,y,method = "lda")

cerebellum <- lda_model$finalModel$means[1,]
hippocampus <- lda_model$finalModel$means[2,]

data.frame(cerebellum,hippocampus) %>% ggplot(aes(x=rownames(.), y=colnames(.)))
