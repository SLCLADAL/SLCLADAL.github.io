
# "Collostructional Analysis"
# "UQ SLC Digital Team"
#
# clean current workspace
rm(list=ls(all=T))
# set options
options(stringsAsFactors = F)         # no automatic data transformation
options("scipen" = 100, "digits" = 4) # supress math annotation
# install libraries
install.packages(c("collostructions"))
# load library
library(collostructions)
# inspect inbuilt data
str(goVerb); head(goVerb)
sum(goVerb$CORP.FREQ)
collex(goVerb, corpsize = 1e+08L, am = "logl", reverse = FALSE, decimals = 5,
       threshold = 1, cxn.freq = NULL, str.dir = FALSE)
#collex.covar(goVerb, am = "logl", raw = TRUE, all = FALSE,
#            reverse = FALSE, decimals = 5, str.dir = FALSE)
#collex.dist(goVerb, am = "logl", raw = FALSE, reverse = FALSE, decimals = 5,
#           threshold = 1, cxn.freqs = NULL, str.dir = FALSE)
