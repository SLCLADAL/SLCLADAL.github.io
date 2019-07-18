###############################################################
# Title:       ConcR_2.3_loadedfiles
# Author:      Martin Schweinberger
# R version:   3.4.1 (2017-06-30) -- "Single Candle
# Date:        2018-11-06
# Contact:     martin.schweinberger.hh@gmail.com
# Description: R script defining function "ConcR" - a concordance function for R
#              The function defined in this script extracts concordances from 
#              strings (i.e. typically already loaded corpus data).
# License:    This script is made available under the GNU General Public License
#             <http://www.gnu.org/licenses/gpl.html>.
# Citation:   Schweinberger, Martin. 2018. ConcR_2.3_loadedfiles.  Concordance 
#             function for R. Unpublished R script. The University of Queensland.
##################################################################
#                            START
##################################################################
# Start defining function
ConcR <- function(strings, pattern, context, all.pre = FALSE) {
  # install required packages
  #install.packages("stringr")
  #install.packages("plyr")
  # activate packages
  require(stringr)
  require(plyr)
  # list files
  conc <- sapply(strings, function(x) {
    txt <- str_replace_all(x, " {2,}" , " ")
    txt <- str_trim(txt, side = "both")
    lngth <- as.vector(unlist(nchar(txt)))
    idx <- str_locate_all(txt, pattern)
    idx <- idx[[1]]
    idx1 <- as.vector(unlist(idx[,1]))
    idx2 <- as.vector(unlist(idx[,2]))
    token.idx.start <- idx1
    token.idx.end <- idx2
    pre.idx.start <- token.idx.start-context
    pre.idx.end <- token.idx.start-1
    pre.idx.all.start <- as.vector(unlist(rep(1, length(idx1))))
    pre.idx.all.end <- idx1-1
    post.idx.start <- idx2+1
    post.idx.end <- idx2+context
    enddf <- cbind(post.idx.end, c(rep(lngth, length(idx2))))
    end <- apply(enddf, 1, function(x){
      ifelse(x[1] > x[2], x <- x[2], x <- x[1]) } )
    post.idx.end <- as.vector(unlist(end))
    startdf <- cbind(post.idx.start, c(rep(1, length(idx1))))
    start <- apply(startdf, 1, function(x){
      ifelse(x[1] > x[2], x <- x[1], x <- x[2]) } )
    post.idx.start <- as.vector(unlist(start))
    conc.df <- cbind(pre.idx.start, pre.idx.end, token.idx.start, token.idx.end, 
      post.idx.start, post.idx.end, pre.idx.all.start)
    concr <- function(conc.df, txt){
      conc <- apply(conc.df, 1, function(x){
        pre <- substr(txt, x[1], x[2])
        token <- substr(txt, x[3], x[4])
        post <- substr(txt, x[5], x[6])
        allpre <- substr(txt, x[7], x[2])
        tbc <- as.vector(unlist(c(rep(length(pre)), pre, token, post, allpre)))
        return (tbc)
        } )
      conc <- as.matrix(conc, nrow = length(idx1), byrow = F)
      return(conc) 
    } 
    conc <- concr(conc.df, txt)
    conc <- t(conc)
    return(conc)
    })
  df <- ldply(conc, data.frame)
  ifelse(nrow(df) >= 1, df <- df[,c(1, 3:ncol(df))], df <- df)
  colnames(df) <- c("OriginalString", "PreContext", "Token", "PostContext", "EntirePreContext")
  df <- df[is.na(df$Token) == F,]
  ifelse(all.pre == FALSE, df <- df[, 1:4], df <- df)
  rownames(df) <- 1:nrow(df)
  return(df)
}
###############################################################
### ---                    THE END
###############################################################
# Example
# list files in a corpus
#files <- list.files("D:\\Uni\\Korpora\\Original\\ICE Ireland version 1.2.2\\ICE-Ireland txt\\ICE spoken running txt", full.names = T)
# use sample
#files <- files[1:5]
# load corpus 
#corpus <- sapply(files, function(x) {
#  x <- scan(x, what = "char", sep = "", quote = "", quiet = T, skipNul = T)
#  x <- paste(x, sep = " ", collapse = " ")
#  x <- unlist(x)
#} )
# inspect structure
#str(corpus)
# perform search
#test <- ConcR(corpus, pattern = " the ", context = 20, all.pre = F)
#head(test)
