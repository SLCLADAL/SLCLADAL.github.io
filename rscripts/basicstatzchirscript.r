
# "The Chi-Square Family of Tests"
# "UQ SLC Digital Team"
#
# clean current workspace
rm(list=ls(all=T))
# set options
options(stringsAsFactors = F)
# install libraries
install.packages(c("stringr", "cfa", "dplyr"))
library(knitr)
chidata <- matrix(c(181, 655, 177, 67), nrow = 2, byrow = T)
# add column and row names
colnames(chidata) <- c("BrE", "AmE")
rownames(chidata) <- c("kindof", "sortof")
kable(chidata, caption = "Observed frequencies of *sort of* and *kind of* in American and British English")r echo = F, results = 'asis'}
library(knitr)
chidata_extended <- matrix(c(181, 177, 358, 655, 67, 722, 836, 244, 1080), nrow = 3, byrow = F)
# add column and row names
colnames(chidata_extended) <- c("BrE", "AmE", "Total")
rownames(chidata_extended) <- c("kindof", "sortof", "Total")
kable(chidata_extended, caption = "Observed frequencies of *sort of* and *kind of* in American and British English with row and column totals")r echo = F, results = 'asis'}
library(knitr)
chidata_expected <- matrix(c(277.1185, 80.88148, 358, 558.8815,163.11852, 722, 836, 244, 1080), nrow = 3, byrow = F)
# add column and row names
colnames(chidata_expected) <- c("BrE", "AmE", "Total")
rownames(chidata_expected) <- c("kindof", "sortof", "Total")
kable(chidata_expected, caption = "Expected frequencies of *sort of* and *kind of* in American and British English with row and column totals")r echo = F, results = 'asis'}
library(knitr)
chidata_chi <- matrix(c(33.33869, 114.22602, 147.5647, 16.53082, 56.63839, 73.16921, 49.86951, 170.8644, 220.7339), nrow = 3, byrow = F)
# add column and row names
colnames(chidata_chi) <- c("BrE", "AmE", "Total")
rownames(chidata_chi) <- c("kindof", "sortof", "Total")
kable(chidata_chi, caption = "Chi values of *sort of* and *kind of* in American and British English with row and column totals")r echo = F, results = 'asis'}
library(knitr)
critval <- matrix(c(1, 3.84, 6.64, 10.83, 2, 5.99, 9.21, 13.82, 3, 7.82, 11.35, 16.27, 4, 9.49, 13.28, 18.47, 5, 11.07, 15.09, 20.52), ncol = 4, byrow = T)
# add column names
colnames(critval) <- c("DF", "p<.05", "p<.01", "p<.001")
kable(critval, caption = "Critical chi values for 1 to 5 degrees of freedom")
chidata              # inspect data
assocplot(as.matrix(chidata))   # association plot
mosaicplot(chidata, shade = TRUE, type = "pearson", main = "")  # mosaic plot
chisq.test(chidata, corr = F)  # perform chi square test
# calculate effect size
sqrt(chisq.test(chidata, corr = F)$statistic / sum(chidata) * (min(dim(chidata))-1))
library(knitr)
critval <- matrix(c("Young", 61, 43, 104, "Old", 42, 36, 78, "Total", 103, 79, 182), ncol = 4, byrow = T)
# add column names
colnames(critval) <- c("", "1SGPN", "PN without 1SG", "Total")
kable(critval, caption = "Table adapted from Gries (2014: 9)")
library(knitr)
library(kableExtra)
critval <- matrix(c("whatever", 17, 55, 71, "other words", 345128, 916552, 1261680, "Total", 345145, 916607, 1261752), ncol = 4, byrow = T)
# add column names
colnames(critval) <- c("", "YoungMales", "YoungFemales", "Total")
kable(critval, caption = "Observed frequency with row- and column totals for the use of *whatever* by male and female speakers.") %>%
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
library(knitr)
critval <- matrix(c("kind of", 32.9927, 113.0407, 146.0335, "sort of", 16.3593, 56.0507, 72.4100, "Total", 49.3520, 169.0914, 218.4434), ncol = 4, byrow = T)
# add column names
colnames(critval) <- c("Variant", "BrE", "AmE", "Total")
kable(critval, caption = "Corrected chi-square values for sort of and kind of in BrE and AmE")r echo = F, results = 'asis'}
library(knitr)
critval <- matrix(c("X-ray soft",  21, 14, 35, "X-ray hard", 18, 13, 31, "Beta-rays", 24, 12, 36, "Light", 13, 30, 43, "Total", 76, 69, 145), ncol = 4, byrow = T)
# add column names
colnames(critval) <- c("", "Mitosis not reached", "Mitosis reached", "Total")
kable(critval, caption = "Data adapted from Bortz (1990: 126)")
# create tdata
wholetable <- matrix(c(21, 14, 18, 13, 24, 12, 13, 30), byrow = T, nrow = 4)
colnames(wholetable) <- c("reached", "notreached")           # add column names
rownames(wholetable) <- c("rsoft", "rhard", "beta", "light") # add row names
wholetable                                                   # inspect data
subtable <- wholetable[1:2,] # extract subtable
subtable                     # inspect subtable
# simple x2-test
chisq.test(subtable, corr = F)
# load function for correct chi-square
source("https://slcladal.github.io/rscripts/x2.2k.r") 
x2.2k(wholetable, 1, 2)
library(knitr)
critval <- matrix(c("chi-squared", 0.0255, 0.025, "p-value", 0.8732, 0.8744), ncol = 3, byrow = T)
# add column names
colnames(critval) <- c("", "chi-square" , "chi-square in 2*k-tables")
kable(critval, caption = "Table adapted from Bortz (1990: 126)")
# create table
wholetable <- matrix(c(8, 31, 44, 36, 5, 14, 25, 38, 4, 22, 17, 12, 8, 11, 16, 24), ncol=4)
attr(wholetable, "dimnames")<-list(Register=c("acad", "spoken", "fiction", "new"),
Metaphor = c("Heated fluid", "Light", "NatForce", "Other"))r echo = F, results = 'asis'}
library(knitr)
critval <- matrix(c("acad", 8, 5, 4, 8, "spoken", 31, 14, 22, 11, "fiction", 44, 25, 17, 16, "new", 36, 38, 12, 24), ncol = 5, byrow = T)
# add column names
colnames(critval) <- c("Register", "Heated fluid", "Light", "NatForce", "Other")
kable(critval, caption = "Table adapted from Gries (2014: 9)")
# create table
subtable <- matrix(c(14, 25, 22, 17), ncol=2)
chisq.results <- chisq.test(subtable, correct=FALSE) # WRONG!
phi.coefficient = sqrt(chisq.results$statistic / sum(subtable) * (min(dim(subtable))-1))
chisq.results
phi.coefficient
# load function for chi square test for subtables
source("https://slcladal.github.io/rscripts/sub.table.r") 
# apply test
results <- sub.table(wholetable, 2:3, 2:3, out="short")
# inspect results
results
# load library
library(cfa)
# load data
cfadata <- read.delim("https://slcladal.github.io/data/cfadata.txt", 
                      header = T, sep = "\t")
# inspect data
head(cfadata)
# load library
library(dplyr)
# define configurations
configs <- cfadata %>%
  select(Variety, Age, Gender, Class)
# define counts
counts <- cfadata$Frequency
# perform cfa
cfa(configs,counts) 
# load library
library(cfa)
# load data
cfadata <- read.delim("https://slcladal.github.io/data/cfadata.txt", 
                      header = T, sep = "\t")
# inspect data
head(cfadata)
# load library
library(dplyr)
# define configurations
configs <- cfadata %>%
  select(Variety, Age, Gender, Class)
# define counts
counts <- cfadata$Frequency
# perform cfa
hcfa(configs,counts) 
According to the HCFA, only a single configuration (Variety : Age : Class) is significant (X2 = 12.21, p = .016). 
# References
