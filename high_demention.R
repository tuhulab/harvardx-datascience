library(devtools)
install_github("genomicsclass/GSE5859Subset")
library(GSE5859Subset)
data(GSE5859Subset)

dim(geneExpression)
dim(sampleInfo)
dim(geneAnnotation)

head(sampleInfo)
sampleInfo$group

match(sampleInfo$filename,
      colnames(geneExpression))

match(geneAnnotation$PROBEID,
      rownames(geneExpression))

# how many samples were processed in "2005-06-27"
sum(sampleInfo$date=="2005-06-27")

library(tidyverse)

left_join(geneExpression,sampleInfo,by="filename")
