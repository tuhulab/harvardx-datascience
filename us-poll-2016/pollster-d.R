library(dslabs)
library(tidyverse)
data("polls_us_election_2016")

polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-31" & state == "U.S.") 

# Create a new object called `pollster_results` that contains columns for pollster name, end date, d_hat, lower confidence interval of d_hat, and upper confidence interval of d_hat for each poll.
pollster_results <- polls %>% 
  mutate(d_hat=rawpoll_clinton/100-rawpoll_trump/100,
         X_hat=(d_hat+1)/2,
         se_hat=2*sqrt(X_hat*(1-X_hat)/samplesize),
         lower=d_hat-qnorm(0.975)*se_hat,
         upper=d_hat+qnorm(0.975)*se_hat) %>% 
  select(pollster,enddate,d_hat,lower,upper)

# Add a logical variable called `hit` that indicates whether the actual value (0.021) exists within the confidence interval of each poll. Summarize the average `hit` result to determine the proportion of polls with confidence intervals include the actual value. Save the result as an object called `avg_hit`.
avg_hit <- pollster_results %>% mutate(hit=lower<=0.021 & upper>=0.021) %>% summarize(mean(hit))

# Add variable called `error` to the object `polls` that contains the difference between d_hat and the actual difference on election day. Then make a plot of the error stratified by pollster.
errors <- pollster_results %>% mutate (errors=d_hat-0.021)
errors %>% ggplot(aes(x=pollster,y=errors)) + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))