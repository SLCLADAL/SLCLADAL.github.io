
# "Tabulating Data"
# "UQ SLC Digital Team"
#
# clean current workspace
rm(list=ls(all=T))
# set options
options(stringsAsFactors = F)         # no automatic data transformation
options("scipen" = 100, "digits" = 4) # supress math annotation
# install libraries
install.packages(c("rtweet", "ggplot2", "dplyr", "tidytext",
                   "twitteR"))
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
mytable <- read.xlsx("D:\\Uni\\UQ\\LADAL\\SLCLADAL.github.io\\data/testdata1.xlsx", 1)
# show first 6 lines of table
head(mytable)
