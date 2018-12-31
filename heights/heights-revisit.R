# Load the 'dslabs' package and data contained in 'heights'
library(dslabs)
data(heights)

# Make a vector of heights from all males in the population
x <- heights %>% filter(sex == "Male") %>%
  .$height

# Calculate the population average. Print this value to the console.
mean(x)

# Calculate the population standard deviation. Print this value to the console.
sd(x)


# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `N` as the number of people measured
N <- 50

# Define `X` as a random sample from our population `x`
X <- sample(x,N,TRUE)

# Calculate the sample average. Print this value to the console.
mean(X)

# Calculate the sample standard deviation. Print this value to the console.
sd(X)

# Define `se` as the standard error of the estimate. Print this value to the console.
se<- sd(X)/sqrt(N)
print(se)

# Construct a 95% confidence interval for the population average based on our sample. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c(mean(X)-qnorm(0.975)*se,mean(X)+qnorm(0.975)*se)

# Define `mu` as the population average
mu <- mean(x)

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `N` as the number of people measured
N <- 50

# Define `B` as the number of times to run the model
B <- 10000

# Define an object `res` that contains a logical vector for simulated intervals that contain mu
res <- replicate(B,
{ X<- sample(x,N,TRUE)
interval <- c(mean(X)-qnorm(0.975)*sd(X)/sqrt(N),mean(X)+qnorm(0.975)*sd(X)/sqrt(N))
between(mu,interval[1],interval[2])
})


# Calculate the proportion of results in `res` that include mu. Print this value to the console.
mean(res)
