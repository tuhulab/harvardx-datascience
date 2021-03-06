library(tidyverse)
library(dslabs)
data("tissue_gene_expression")
d <- dist(tissue_gene_expression$x - rowMeans(tissue_gene_expression$x)) 
h <- hclust(d)
plot(h, cex = 0.65)
