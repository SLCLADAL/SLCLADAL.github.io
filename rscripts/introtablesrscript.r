
# "Tabulating Data"
# "UQ SLC Digital Team"
#
# clean current workspace
rm(list=ls(all=T))
# set options
options(stringsAsFactors = F)         # no automatic data transformation
options("scipen" = 100, "digits" = 4) # supress math annotation
# install libraries
install.packages(c("xlsx", "dplyr", "tidyr"))
# load data with read.delim
mytable <- read.delim("https://slcladal.github.io/data/mlrdata.txt", header = TRUE)
# show first 6 lines of table
head(mytable)
# load data with read.delim
myothertable <- read.table("https://slcladal.github.io/data/mlrdata.txt", header = TRUE)
# show first 6 lines of table
head(myothertable)
# load library
library(xlsx)
# load table into R
# WARNING! set your own path!
exceldata <- read.xlsx("data/testdata1.xlsx", 1)
# show first 6 lines of table
head(exceldata)
# load libraries
library(dplyr)
library(tidyr)
# load new data
newdata <- read.delim("data/mlrdata.txt", sep = "\t", header = T)
# inspect data before processing
nrow(newdata); str(newdata); table(newdata$attraction)
# example of a data processing pipeline
pipeddata <- read.delim("data/mlrdata.txt", sep = "\t", header = T) %>%
  dplyr::rename(Status = status, Attraction = attraction, Money = money) %>%
  dplyr::group_by(Status, Attraction) %>%
  dplyr::summarise(Mean = mean(Money))
# inspect summarized data
pipeddata
# select and filter
reduceddata <- newdata %>%
  # select the columns attraction and money
  dplyr::select(attraction, money) %>%
  # extract rows which represent cases where the person was intersted in someone
  dplyr::filter(attraction == "Interested")
# inspect new table
nrow(reduceddata); table(reduceddata$attraction)
# select and filter
datawithoutmoney <- newdata %>%
  # remove money
  dplyr::select(-money) 
# inspect data
head(datawithoutmoney)
# creating a new column
newdata <- newdata %>%
  dplyr::mutate(Spendalot = ifelse(money >= 100, "Alot", "Alittle")) 
# inspect data
head(newdata)
# renaming columns
newdata <- newdata  %>%
  dplyr::rename(Status = status, Attraction = attraction, Money = money)
# inspect data
head(newdata)
#grouping and summarizing data 
datasummary <- newdata %>%
  dplyr::group_by(Status, Attraction) %>%
  dplyr::summarise(Mean = round(mean(Money), 2), SD = round(sd(Money), 1))
# inspect summarized data
datasummary
# converting data to wide format 
widedata <- datasummary %>%
  # remove SD column
  dplyr::select(-SD) %>% 
  # convert into wide format
  tidyr::spread(Attraction, Mean)
# inspect wide data
widedata
# converting data to long format 
longdata <- widedata %>%
  # convert into long format
  tidyr::gather(Attraction, Money, Interested:NotInterested)
# inspect wide data
longdata
There are many more useful functions for processing, handling, and summarizing tables but this should suffice to get you started.
