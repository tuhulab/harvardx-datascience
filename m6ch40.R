library(tidyverse)
library(rvest)
url <- paste0("https://en.wikipedia.org/w/index.php?title=",
              "Gun_violence_in_the_United_States_by_state&direction=prev&oldid=810166167")
murders_raw <-  read_html(url) %>%
  html_nodes('table') %>%
  html_table() %>%
  .[[2]] %>%
  setNames(c('state','population','total','murder_rate'))
head(murders_raw)

murders_raw$population %>% parse_number

commas <- function (x) {
  any(str_detect(x,','))
}
murders_raw  %>% summarize_all(funs(commas))

test_1 <- murders_raw$population %>% str_replace_all(pattern = ',',replacement = '')
test_1 <- as.numeric(test_1)

test_2 <- parse_number(murders_raw$population)
identical(test_1,test_2)

murders_new <- murders_raw %>% mutate_at(2:3,parse_number)

#case 2
library(dslabs)
data("reported_heights")
str(reported_heights$height)
x <- as.numeric(reported_heights$height)
sum(is.na(x))
#reported_heights %>% select(height) %>% filter(is.na(x))
reported_heights %>% 
  mutate(new_height = as.numeric(height)) %>%
  filter(is.na(new_height)) %>% head()

not_inches <- function(x,smallest=50,tallest=84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
}

problems <- reported_heights %>% 
  filter(not_inches(height)) %>%
  .$height
  
length(problems)

sum(str_detect(reported_heights$height,','))

sum(str_detect(reported_heights$height,'cm'))
str_subset(reported_heights$height,'cm')

yes <- c("AB", "A1B", "A11B", "A111B", "A1111B")
no <- c("A2B", "A21B")
str_detect(yes, "A1*B")
