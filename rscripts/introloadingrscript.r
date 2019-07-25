
# "Loading and Exporting Data"
# "UQ SLC Digital Team"
#
# clean current workspace
rm(list=ls(all=T))
# set options
options(stringsAsFactors = F)         # no automatic data transformation
options("scipen" = 100, "digits" = 4) # supress math annotation
# install libraries
install.packages(c("cluster", "factoextra", "cluster", 
                   "seriation", "pvclust", "ape", "vcd", 
                   "exact2x2", "factoextra", "seriation", 
                   "NbClust", "pvclust"))
# Setting workspace
#setwd("D:\\StatisticsForLinguists")
# show workspace
getwd ()
library(xlsx)
install.packages("xlsx")
library(xlsx)
# define path to data
path <- "data/testdata1.xlsx"
# load data with defined path
mydata <- read.xlsx(path, 1)
# load data without pre-defining the path
mydata <- read.xlsx("data/testdata1.xlsx", 1)
mydata <- read.xlsx(choose.files(), 1)
mydata
summary(mydata)
str(mydata)
head(mydata)
mydata <- read.table(choose.files(), header = T, sep = "\t", quote = "", comment.char = "")
# define path to corpus
corpuspath <- "D:\\Uni\\UQ\\LADAL\\SLCLADAL.github.io\\data\\testcorpus"
# define files to load
corpus.files = list.files(path = corpuspath, pattern = NULL, all.files = T,
  full.names = T, recursive = T, ignore.case = T, include.dirs = T)
# load corpus and start processing
corpus <- lapply(corpus.files, function(x) {
  x <- scan(x, what = "char", sep = "", quote = "", quiet = T, skipNul = T)
  x <- paste(x, sep = " ", collapse = " ")
  } )
# inspect first file
corpus[[1]]
# inspect corpus structure
str(corpus)
# inspect corpus
summary(corpus)
# define path to data
testdatawords <- paste(as.vector(unlist(corpus)), sep = " ", collapse = " ")
testdatawords <- gsub("[^[:alpha:][:space:]]*", "", testdatawords)
testdatawords <- as.vector(unlist(strsplit(testdatawords, " ")))
testdatawords <- table(testdatawords)[order(table(testdatawords), decreasing = T)]
head(testdatawords)
# define path to data
outpath <- "data/testdatawords.txt"
# save data to pc
write.table(testdatawords, file = outpath, sep = "\t", col.names = TRUE, row.names = F, quote = F)
library(xlsx)
write.xlsx (testdatawords, "data/testdatawords.xlsx")
