library(tidyverse)
library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()

class(mnist$train$images)
x <- mnist$train$images[1:1000,]
y <- mnist$train$labels[1:1000]

x_1 <- 1:5
x_2 <- 6:10
cbind(x_1,x_2)

dim(x) #nrow,ncol
dim(x_1)
dim(as.matrix(x_1))

x <- matrix(rnorm(100*10), 100, 10)
dim(x)
nrow(x)
ncol(x)

my_vector <- 1:3
mat <- matrix(my_vector, nrow = 3, ncol=5, byrow = TRUE)

#exercise_put a vector into a matrix
dim(x)
dim(as.matrix(x[3,])) #this one is one predictor!
dim(as.matrix(x[,3])) #this one does not make sense
dim(as.matrix(y))

grid <- matrix(x[3,],28,28)
image(1:28,1:28,grid)
image(1:28,1:28,grid[,28:1])

sums <- rowSums(x)
avg <- rowMeans(x)
data.frame(labels=as.factor(y), row_averages=avg) %>%
  qplot(labels,row_averages,data=.,geom="boxplot")

data.frame(labels=as.factor(y), row_averages=avg) %>%
  ggplot(aes(x=labels,y=row_averages)) + geom_boxplot() + geom_point()

data.frame(labels=as.factor(y), row_sums=sums) %>%
  ggplot(aes(x=labels,y=row_sums)) + geom_boxplot() + geom_point() ## because they have same predictors. sum plot = avg plot

colsums <- colSums(x) #pixel
colsds <- colSds(x)

#test_find median value of each col or row
row_median <- apply(x,1,median) #median value of each subject 
col_median <- apply(x,2,median) #median value of each pixel's 


library(matrixStats)
sds <- colSds(x) #sd of 784 pixels
qplot(sds,bins="30",color=I("black"))

image(1:28,1:28,matrix(sds,28,28)[,28:1])

new_x <- x[,colSds(x)>60]
dim(new_x)
class(x[,1:2])
class(x[,1,drop=FALSE])


mat <- matrix(1:15,5,3)
as.vector(mat)

qplot(as.vector(x),bins=30, color=I("black"))

#remove some pixels 
#do not overwrite original x
new_x <- x
new_x[new_x < 50] <- 0
qplot(as.vector(new_x),bins=30, color=I("black"))

mat <- matrix(1:15, 5, 3)
mat[mat < 3] <- 0
mat
qplot(as.vector(mat),bins=15, color=I("black"))

mat <- matrix(1:15, 5, 3)
mat[mat > 6 & mat < 12] <- 0
mat


#binarizing the data
bin_x <- x
bin_x[bin_x < 255/2] <- 0
bin_x[bin_x > 255/2] <- 1


#use linear algebra to scale matrix
(x-rowMeans(x))/rowSds(x)


#exericse
x <- matrix(1:15,3,5)
sweep(x, 2, 1:ncol(x),"+")

#exercise-Q6
sum((x>50 & x <205)==TRUE)/(dim(x)[1]*dim(x)[2])


set.seed(1995)
ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)
x <- mnist$train$images[ind,]
y <- mnist$train$labels[ind]




