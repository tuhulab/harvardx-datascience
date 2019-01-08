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
#in R, file.path is used insted of paste due to convenience

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
