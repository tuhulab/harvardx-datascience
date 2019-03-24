library(dslabs)
y <- read_mnist()

library(tidyverse)
library(caret)
library(dslabs)
data(heights)
y <- heights$sex
x <- heights$height
set.seed(2007)
test_index <- createDataPartition(y, 
                                  times = 1, #how many indexes we wish to return
                                  p = 0.5, 
                                  list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

y_hat <- sample(c("Male", "Female"), 
                length(test_index), 
                replace = TRUE)  %>% factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)

#get a summary of height data
heights %>% group_by(sex) %>% summarize(mean(height), sd(height))

#algorithm-1: higher than 62 classified as Male
y_hat <- ifelse(x > 62, "Male", "Female") %>% factor(levels = levels(test_set$sex))
mean(y == y_hat)
