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


###disease
set.seed(1)
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

#test returns positive results
mean(test==1)

#test returns positive results, while disease
mean(disease[test==1]==1)

#disease returns negative
mean(disease[test==0]==1)

#relative risk of test pos but disease
mean(disease[test==1]==1)/0.02

##############smoothing############
library(tidyverse)
library(dslabs)
data("polls_2008")
qplot(day, margin, data=polls_2008)
