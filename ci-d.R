library(dslabs)
library(tidyverse)

# Load the data
data(polls_us_election_2016)

# Add a statement to this line of code that will add a new column named `d_hat` to `polls`. The new column should contain the difference in the proportion of voters.
polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-31" & state == "U.S.") 


# Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.
N <- polls[1,]$samplesize
print(N)

# For the difference `d_hat` of the first poll in `polls` to a variable called `d_hat`. Print this value to the console.
d_hat <- polls[1,] %>% mutate(d_hat=rawpoll_clinton/100-rawpoll_trump/100)
d_hat <- d_hat$d_hat
print(d_hat)

# Assign proportion of votes for Clinton to the variable `X_hat`.
X_hat <- (d_hat+1)/2

# Calculate the standard error of the spread and save it to a variable called `se_hat`. Print this value to the console.
se_hat <- 2*sqrt(X_hat*(1-X_hat)/N)
print(se_hat)

# Use `qnorm` to calculate the 95% confidence interval for the difference in the proportions of voters. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c(d_hat-qnorm(0.975)*se_hat,d_hat+qnorm(0.975)*se_hat)