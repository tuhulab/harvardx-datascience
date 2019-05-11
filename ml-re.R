library(tidyverse)
library(caret)

library(dslabs)
data(heights)

y <- heights$sex #outcome
x <- heights$height #predictors(feature, covariants)

set.seed(1)
test_index <- createDataPartition(y,
                                  times=1,
                                  p=0.5,
                                  list=FALSE)
test_set <- heights[test_index,]
train_set <- heights[-test_index,]

# algorithm_1_guessing
y_hat <- sample(x = c("Male","Female"),
                size= nrow(train_set), replace = TRUE)
mean(y_hat == test_set$sex)

#algorithm_2_using_cutoff
heights %>% group_by(sex) %>% summarize(mean(height),sd(height))

  ##if height is within 2-sd, classified as male
    y_hat <- ifelse(x>62.08, "Male", "Female")
    mean(y==y_hat)
  
  ##try more cutoff
    #cutoff <- seq(from=min(heights$height), to=max(heights$height),length=100)
    cutoff_book <- seq(from=61, to=70)

    n <- 1:length(cutoff_book)
    
    compare <- function(number=...){
      y_hat <-ifelse(x>cutoff_book[number], "Male", "Female")
      accuracy <-mean(y_hat == y)
    }
    
    result <- sapply(n,FUN = compare)
    cutoff_accuratcy<- data_frame(cutoff_book,result)
    ggplot(cutoff_accuratcy,aes(x=cutoff_book,y=result)) + geom_point(alpha=1) + geom_line()
    
    max(result)
    cutoff_book[which.max(result)]    
    