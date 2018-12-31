library(dslabs)
library(caret)
data(heights)

#extract features and outcomes
x <- heights$height
y <- heights$sex

#splite the heights into training set & test set
set.seed(1)
index <- createDataPartition(y)