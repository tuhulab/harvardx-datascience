library(dslabs)
library(caret)
library(tidyverse)
library(e1071)
data(heights)

#extract features and outcomes
x <- heights$height
y <- heights$sex

#split data1 - generate index
set.seed(1)
train_index <- createDataPartition(y,times=1,p=.5,list=FALSE)

#split data2 - data reshaping
x_train <- x[train_index]
x_test <- x[-train_index]

y_train <- y[train_index]
y_test <- y[-train_index]

#algorithm1 - guessing
y_hat1 <- sample(c('Male','Female'),length(x_test),replace=TRUE) %>% factor()
#algorithm1 - assess
result1 <- mean(y_hat1 == y_test)

#algorithm2 - average height
  #We have this insight: male's height is on average than female of this dataset
  heights %>% group_by(sex) %>% summarise(mean(height),sd(height))
  #We build an algorithm that classify heights higher than 62 inch (2 sd lower than mean) as male
  y_hat2 <- ifelse(x_test>62,'Male','Female') %>% factor()
  #Assess this algorithm by overall accuracy
  result2 <- mean(y_hat2 == y_test)

#algorithm2 - determine a better cutoff
  #Generate a cutoff vector - length of 20 from female height to male height 
  femaleheight <- heights %>% filter(sex=='Female')
  femalemeanheight <- mean(femaleheight$height)
  maleheight <- heights %>% filter(sex=='Male')
  malemeanheight <- mean(maleheight$height)
  cutoff <- seq(from=61,to=70,length.out = 500)
  #cutoff <- seq(61,72)
  #Predict
        cutoffcompare <- function(cutoff=...){
          testresult <- ifelse(x_train>cutoff,'Male','Female') %>% factor()
          overallaccuracy = mean (testresult == y_train)
          return(overallaccuracy)
        }
  accuracy <- sapply(cutoff,cutoffcompare)
  predictionresult <- data.frame(cutoff,accuracy)
  predictionresult %>% ggplot(aes(cutoff,accuracy)) + geom_point() + geom_line()
  
  #The best prediction result we can achieve is 
  max(accuracy)
  optimalcutoff <- cutoff[which.max(accuracy)]
  
  #check the cutoff in test set
  y_hat3 <- ifelse(x_test>optimalcutoff,'Male','Female') %>% factor()
  mean(y_hat3==y_test)

#assess algorithm
  table(predicted=y_hat3,actual=y_test)

  result <- data.frame(y_test,y_hat3) %>% mutate(accuracy=y_test==y_hat3) %>% group_by(y_test) %>% summarise(mean(accuracy))
  print(result)

  prev_train <- mean(y_train=='Male');print(prev_train)
  prev_test <- mean(y_test=='Male');print(prev_test)

#data is biased. prediction is biased. overall accuracy is NOT enough to represent the 'quality' of prediction.
#sensitivity and specificity
  confusionMatrix(data = y_hat3,reference=y_test)

#keep sensitivity & specificity in one variable is also importnat for optimization, e.g. F1.
#This time we want to achieve best F1 value rather than best overall accuracy.
  cutoffcompare <- function(cutoff=...){
    y_hat4 <- ifelse(x_train>cutoff,'Male','Female') %>% factor()
    fvalue <- F_meas(y_hat4,reference = factor(y_train))
    return(fvalue)}
  fvalue <- sapply(cutoff,cutoffcompare)
  predictionresult2 <- data.frame(cutoff,fvalue)
  predictionresult2 %>% ggplot(aes(cutoff,fvalue)) + geom_point() + geom_line()
  
  print(max(fvalue)) 
  cutoff[which.max(fvalue)]
#be careful of train/test set
  
  
#plot the ROC curve
  
  
  

  library(dslabs)
  data("heights")
  heights %>% 
    mutate(height = round(height)) %>%
    group_by(height) %>%
    summarize(p = mean(sex == "Male")) %>%
  qplot(height, p, data =.)
  
  ps <- seq(0, 1, 0.1)
  heights %>% 
    mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
    summarize(p = mean(sex == "Male"), height = mean(height)) %>%
    qplot(height, p, data =.)
  
  Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
  dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  plot(dat)
  ps <- seq(0, 1, 0.1)
  dat %>% 
    mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
    group_by(g) %>%
    summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data =.)
  
  