library(tidyverse)

#simulate a tibble
  ##rnorm:random generation
df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

 ##calculate median value. However, it's not nice. Because rule of thumb is: never copy more than twice
median(df$a)
median(df$b)
median(df$c)
median(df$d)

 ## write a loop
#define output space (good practise to define output first)
  output <- vector("double",ncol(df))
  for (i in seq_along(df)) {          
    output[[i]] <- median(df[[i]])
  }
  output

 ## mtcars example
  output <- vector("numeric",ncol(mtcars))
  for (i in seq_along(mtcars)){
    output[[i]] <- mean(mtcars[[i]])
  }
  output
  
 ## nycflights13::flights
  library(nycflights13)
  output <- vector(length=ncol(flights))
    