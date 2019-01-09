library(tidyverse)
library(dslabs)

#current working directory. it is platform-dependent.
getwd()

#get the path of a file in R package
path <- system.file("extdata", package="dslabs")
list.files(path)

#move a file to local directory
filename <- 'murders.csv'
fullpath <- file.path(path,filename)
fullpath1 <- paste(path,filename,sep='/')
identical(fullpath,fullpath1)
#in R, file.path is used insted of paste due to convenience (in different OS, different path formate)

#copy files
file.copy(from = fullpath, to = getwd())
file.exists('murders.csv')

localpath <- file.path(getwd(),filename)

#read funciton (the difference of tidyverse package and R base package. '_' and '.')
murders <- read.csv(localpath)
class(murders)
head(murders)
class(murders$region)

murders.1 <- read.csv(localpath,stringsAsFactors = FALSE)
class(murders.1$region)


murders_1 <- read_csv(localpath)
class(murders_1)
head(murders_1)

#download through url
url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"

murders_url <- read_csv(url)

download.file(url, destfile = file.path(getwd(),'murders_through_url.csv'))

path_url <- file.path(getwd(),'murders_through_url.csv')
murders_url_local <- read_csv(path_url)
identical(murders_url,murders_url_local)

#reshape data (practise of gather, spreed, separate)

#the tidy data
data('gapminder')
tidy_data <- gapminder %>% 
  filter(country %in% c('Germany','South Korea')) %>%
  select(country,year,fertility)
head(tidy_data)
#plot
tidy_data %>% ggplot(aes(x=year,y=fertility,color=country)) + 
  geom_point() 

plotapply <- function(data){
  data %>% ggplot(aes(x=year,y=fertility,color=country)) + 
    geom_point() 
}

#load the wide data
path <- system.file('extdata',package='dslabs')
filename <- file.path(path,'fertility-two-countries-example.csv')
wide_data <- read_csv(filename)
head(wide_data)

tidy_data_by_user <- wide_data %>% gather(year,fertility,-country)
head(tidy_data)
plotapply(tidy_data_by_user)

#In real life, data could be formated even worse
path <- system.file('extdata',package='dslabs')
list.files(path)
filename <- 'life-expectancy-and-fertility-two-countries-example.csv'
fullpath <- file.path(path,filename)
#read
wide_data2 <- read_csv(fullpath)
#data reshape
tidy_data_2 <- wide_data2 %>% gather(key,value,-country)
tidy_data_2 %>% separate(key,c('year','variable_name'),'_')
tidy_data <- tidy_data_2 %>% separate(key,c('year','variable_name'),'_',extra='merge') %>% spread(variable_name,value)
tidy_data_2 %>% separate(key,c('year','first_variable_name','second_variable_name'),fill='right') %>% 
  unite(col = variable_name,first_variable_name,second_variable_name,sep='_') %>% 
  spread(variable_name,value) %>% rename(fertility=fertility_NA)
