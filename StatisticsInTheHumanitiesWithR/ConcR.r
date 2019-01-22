###############################################################
### --- R script defining function "ConcR" - a concordance function for R
### --- Author: Martin Schweinberger (12/2014)
### --- R version 3.1.0 (2014-04-10) -- "Spring Dance"
### --- This script was written by Martin Schweinberger
### --- (<http://www.martinschweinberger.de/blog/>).
### --- It extracts concordances from corpus data.
### --- This script is made available under the GNU General Public License
### --- <http://www.gnu.org/licenses/gpl.html>.
### --- If you use it, PLEASE QUOTE it as:
### --- Schweinberger, Martin. 2014. ConcR - a concordance function for R.
### --- Author: Martin Schweinberger (12/2014).
### --- Unpublished R script.
### --- THANK YOU. Copyright Martin Schweinberger (2014).
##################################################################
##################################################################
### --- START
##################################################################
# Start defining function
# without output saved on computer
ConcR <- function(pathname, search.pattern, context, all.pre = FALSE) {
# with output saved on computer
#ConcR <- function(pathname, outputpath, search.pattern, context, file.range = "all", all.pre = "no") {
# Install required packages
  #install.packages("plyr")
  #install.packages("data.table")
  #install.packages("stringr")
  #install.packages("tm")
  # Load packages
  library(plyr)
  library(data.table)
  library(stringr)
  library(tm)
# 1) Load file IDs
corpus.files = list.files(path = pathname, pattern = NULL, all.files = T,
  full.names = T, recursive = T, ignore.case = T, include.dirs = T)
###############################################################
# Tokenize the corpus files
corpus.tmp <- sapply(corpus.files, function(x) {
  x <- scan(x, what = "char", sep = "\n", quiet = T)
  x <- gsub("\t" , " ", x, perl= T, fixed = T)
  x <- gsub(" {2,}" , " ", x)
  x <- str_trim(x, side = "both")
  x <- unlist(strsplit(x, " "))  }  )
# Extract the positions of the tokens
concordance.index <- sapply(corpus.tmp, function(x)  {
  x <- grep(search.pattern, x, ignore.case = T, value = F)  }  )
###############################################################
# Extract tokens
token.orig.raw <- sapply(corpus.tmp, function(x) {
  x[grep(search.pattern, x, ignore.case = T, value = F)]  }  )
text.id.raw <- as.vector(unlist(sapply(names(token.orig.raw), function(x) {
  x <- gsub(".*/", "", x)
  x <- gsub("\\ .*", "", x)
  x <- gsub("\\.TXT", "", x)
  x <- gsub("\\.txt", "", x) } )))
len <- as.vector(unlist(sapply(token.orig.raw, function(x) {
  x <- length(x)} )))
text.id <- rep(text.id.raw, len)
token.orig <- as.vector(unlist(token.orig.raw))
###############################################################
# Extract previous elements (limited)
pre.orig <- as.vector(unlist(sapply(corpus.tmp, function(x) {
  position.of.matches <- grep(search.pattern, x, ignore.case = T, value = F)
  sapply(position.of.matches, function(y){
    start <- max(1, as.numeric(y)-context)
    stop <- as.numeric(y)-1
    z <- x[start:stop]
    z <- paste(z, collapse = " ")
    }  )  }  )))
# Extract all previous context
pre.all.orig <- as.vector(unlist(sapply(corpus.tmp, function(x) {
  position.of.matches <- grep(search.pattern, x, ignore.case = T, value = F)
  sapply(position.of.matches, function(y){
    z <- x[1 : y-1]
    z <- paste(z, collapse = " ")
    }  )  }  )))
# Extract following context
post.orig <- as.vector(unlist(sapply(corpus.tmp, function(x) {
  position.of.matches <- grep(search.pattern, x, ignore.case = T, value = F)
  sapply(position.of.matches, function(y){
    end <- y+ context
    z <- x[(y+1):end]
    z <- paste(z, collapse = " ")
    }  )  }  )))
# Create a vector out of the original corpus material surrounding the match
orig.tb <- matrix(cbind(pre.orig, rep("<<", length(pre.orig)), token.orig, rep(">>", length(pre.orig)), post.orig), ncol = 5)
orig <- apply(orig.tb, 1, paste, collapse = " ")
###############################################################
# Clean preceeding content
pre.tmp <- as.vector(unlist(sapply(pre.orig, function(x) {
  sapply(x, function(y) {
    y <- gsub("(<&.*?&>)", "", y)
    y <- gsub("(<\\[[0-9]{0,2}>|<\\]>|<\\{[0-9]{0,2}>|<\\}>|</\\[[0-9]{0,2}>|</\\{[0-9]{0,2}>|<=>|</=>|</->|</\\}>|<->|<\\$[A-Z]>|<\\$[a-z]>)", "", y)
    y <- str_trim(y, side = "both")
    y <- gsub(" {2,}", " ", y)  }  )  }  )))
# Extract all content preceding a match content
pre.all <- as.vector(unlist(sapply(pre.all.orig, function(x) {
  sapply(x, function(y) {
    y <- gsub("(<&.*?&>)", "", y)
    y <- gsub("(<\\[[0-9]{0,2}>|<\\]>|<\\{[0-9]{0,2}>|<\\}>|</\\[[0-9]{0,2}>|</\\{[0-9]{0,2}>|<=>|</=>|</->|</\\}>|<->|<\\$[A-Z]>|<\\$[a-z]>)", "", y)
    y <- str_trim(y, side = "both")
    y <- gsub(" {2,}", " ", y)  }  )  }  )))
# Clean matches
token.tmp <- as.vector(unlist(sapply(token.orig, function(x) {
  sapply(x, function(y) {
    y <- gsub("(<&.*?&>)", "", y)
    y <- gsub("(<\\[[0-9]{0,2}>|<\\]>|<\\{[0-9]{0,2}>|<\\}>|</\\[[0-9]{0,2}>|</\\{[0-9]{0,2}>|<=>|</=>|</->|</\\}>|<->|<\\$[A-Z]>|<\\$[a-z]>)", "", y)
    y <- str_trim(y, side = "both")
    y <- gsub(" {2,}", " ", y)  }  )  }  )))
# Clean subsequent content
post.tmp <- as.vector(unlist(sapply(post.orig, function(x) {
  sapply(x, function(y) {
    y <- gsub("(<&.*?&>)", "", y)
    y <- gsub("(<\\[[0-9]{0,2}>|<\\]>|<\\{[0-9]{0,2}>|<\\}>|</\\[[0-9]{0,2}>|</\\{[0-9]{0,2}>|<=>|</=>|</->|</\\}>|<->|<\\$[A-Z]>|<\\$[a-z]>)", "", y)
    y <- str_trim(y, side = "both")
    y <- gsub(" {2,}", " ", y)  }  )  }  )))
# Create a vector out of the clean corpus material surrounding the match
test.tb <- matrix(cbind(pre.tmp, rep("<<", length(pre.tmp)), token.tmp, rep(">>", length(pre.tmp)), post.tmp), ncol = 5)
test <- apply(test.tb, 1, paste, collapse = " ")
test <-  gsub(" {2,}", " ", test)
test <- str_trim(test, side = "both")
###############################################################
# Create a table of the extracted information
redux <- cbind(1:length(text.id), text.id, pre.tmp, token.tmp, post.tmp, orig, test)
colnames(redux) <- c("id", "text.id", "previous element(s)","token","following element(s)", "orig.data", "test.column")
full <- cbind(1:length(text.id), text.id, pre.all, pre.tmp, token.tmp, post.tmp, orig, test)
colnames(full) <- c("id", "text.id", "previous context", "previous element(s)","token","following element(s)", "orig.data", "test.column")
ifelse(all.pre == FALSE, kwic.tmp <- redux, kwic.tmp <- full)
###############################################################
# Create txt file in which we store the results
#output.file = file.create(outputpath, showWarnings = F)
# Store the txt file in the output file
#write.table(kwic.tmp, outputpath, sep = "\t", row.names = F)
# Return
return(list(c(length(kwic.tmp[, 1])), kwic.tmp))
# End function
  }
###############################################################
### ---                    THE END
###############################################################

# Example
#pathname <- "C:\\PhD\\skripts n data\\corpora\\ICE Ireland version 1.2.2\\ICE-Ireland txt\\ICE spoken running txt"
#outputpath <- "C:\\Users\\msch\\Desktop/kwic ice ireland raw.txt"
#search.pattern <-  c("[L|l]ike")
#context <- 10
#all.pre = TRUE
#test <- ConcR(pathname, search.pattern, context, all.pre = TRUE)
#test[[1]]
#head(test[[2]])
#
