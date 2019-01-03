library(dslabs)
library(caret)
data(heights)

#extract features and outcomes
x <- heights$height
y <- heights$sex

#split data1 - generate index
set.seed(1)
train_index <- createDataPartition(y,times=1,p=.8,list=FALSE)

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

#algorighm2 - determine a better cutoff
  #Generate a cutoff vector - length of 20 from female height to male height 
  femaleheight <- heights %>% filter(sex=='Female')
  femalemeanheight <- mean(femaleheight$height)
  maleheight <- heights %>% filter(sex=='Male')
  malemeanheight <- mean(maleheight$height)
  cutoff <- seq(from=61,to=70,length.out = 50)
  #Predict
        cutoffcompare <- function(cutoff=...){
          testresult <- ifelse(x_test>cutoff,'Male','Female')
          overallaccuracy = mean (testresult == y_test)
          return(overallaccuracy)
        }
  accuracy <- sapply(cutoff,cutoffcompare)
  predictionresult <- data.frame(cutoff,accuracy)
  predictionresult %>% ggplot(aes(cutoff,accuracy)) + geom_point() + geom_line()

  