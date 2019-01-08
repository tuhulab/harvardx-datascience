#build the ure
beads <- rep(c('blue','red'),times=c(3,2))

#sample from the bag
sample(beads,size = 1)

#replicate the sampling
B <- 10000
events <- replicate(B,sample(beads,1))

#summarize the results
tab <- table(events)
tab
proptab <- prop.table(tab)
proptab

#build the card deck
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", 
             "Six", "Seven", "Eight", "Nine", "Ten", 
             "Jack", "Queen", "King")
deck <- expand.grid(suit=suits, number=numbers)
deck <- paste(deck$number,deck$suit)

#double check the probability of a king is 1/13
kings <- paste('King',suits)
mean(kings == deck); mean(deck == kings)
#NOT the correct way. kings vector is recycled

mean(deck %in% kings) ; mean(kings %in% deck)
#first asks in the deck, who is kings?
#seond asks are kings in the deck?

#gtools and permutation function
library(gtools)
permutations(3,2)

#pick 5 random phone numbers from all possible permutations
all_phone_numbers <- permutations(10,7,v=0:9)
n <- nrow(all_phone_numbers)
set.seed(1515)
index <- sample(n,5)
all_phone_numbers[index,]

#what is the pr of 'second is a king given that first is a king'
hands <- permutations(n=52, r=2, v=deck)
first_card <- hands[,1]
second_card <- hands[,2]
sum(first_card %in% kings)

sum(first_card %in% kings & second_card %in% kings) / sum(first_card %in% kings)
mean(first_card %in% kings & second_card %in% kings) / mean(first_card %in% kings)

#combination
combinations(3,2)
combinations(3,1)
combinations(3,3)

#compute 'natural 21' in blackjack
allcombinations <- nrow(combinations(52,2,v=deck))

aces <- paste('Ace',suits)

face <- c('Ten','Jack','Queen','King')
face <- expand.grid(face=face,suits=suits)
face <- paste(face$face,face$suits)

natural21 <- expand.grid(aces=aces,face=face)
natural21 <- paste(natural21$aces,natural21$face)
natural21 <- length(natural21)

natural21/allcombinations

#mean((hands[,1] %in% aces & hands[,2] %in% facecard) |
#       (hands[,2] %in% aces & hands[,1] %in% facecard))

#Monte Carlo Simulation
hand <- sample(deck,2)
hand

(hand[1] %in% aces & hand[2] %in% face) | (hand[2] %in% aces & hand[1] %in% face) 

n <- 10000
result <- replicate(n,{
  hand <- sample(deck,2)
  result <- (hand[1] %in% aces & hand[2] %in% face) | (hand[2] %in% aces & hand[1] %in% face) 
} )
mean(result)

#birthday problem
same_birthday <- function(n){
  bdays <- sample(1:365,n,replace = TRUE)
  any(duplicated(bdays))
}

prob_cal <- function(n){
  results <- replicate(10000,same_birthday(n))
  mean(results)
}

P <- 1:365
result <- sapply(P,prob_cal)
bdproblem <- data.frame(population=P,probability=result)
library(tidyverse)
bdproblem %>% ggplot(aes(x=population,y=probability)) + geom_point()

P1 <- 1:100
result1 <- sapply(P1,prob_cal)
bdproblem1 <- data.frame(population=P1,probability=result1)
bdproblem1 %>% ggplot(aes(x=population,y=probability)) + geom_point()

#How big is big? examplified with 25 people birthday problem
prob_cal <- function(N){
  results <- replicate(N,same_birthday(25))
  mean(results)
}
times <- 1:10000
result25 <- sapply(times,prob_cal)
result25 <- data.frame(repetition=times,probability=result25)

result25 %>% ggplot(aes(x=log(times),y=prob_cal)) + geom_line()


times2 <- 10^seq(1,5,length=100)
result25 <- sapply(times2,prob_cal)
result25 <- data.frame(repetition=times2,probability=result25)

result25 %>% ggplot(aes(x=log10(repetition),y=probability)) + geom_point() + geom_line()
