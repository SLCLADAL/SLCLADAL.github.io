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
    conc.df <- cbind(pre.idx.start, pre.idx.end, token.idx.start, token.idx.end, post.idx.start, post.idx.end, pre.idx.all.start)
    concr1 <- function(conc.df, txt){
    conc <- apply(conc.df, 1, function(x){
      pre <- substr(txt, x[1], x[2])
      token <- substr(txt, x[3], x[4])
      post <- substr(txt, x[5], x[6])
      tbc <- as.vector(unlist(c(rep(length(pre)), pre, token, post)))
      return (tbc)
      } )
    conc <- as.matrix(conc, nrow = length(idx1), byrow = F)
    return(conc) }
    concr2 <- function(conc.df, txt){
    conc <- apply(conc.df, 1, function(x){
          pre <- substr(txt, x[1], x[2])
          token <- substr(txt, x[3], x[4])
          post <- substr(txt, x[5], x[6])
          all.pre <- substr(txt, x[7], x[2])
          tbc <- as.vector(unlist(pre, token, post, all.pre))
          return(tbc)
       } )
       conc <- as.matrix(conc, nrow = length(idx1), byrow = F)
      return(conc)
       }
       ifelse(all.pre == F, conc <- concr1(conc.df, txt), conc <- concr2(conc.df, txt))
      conc <- t(conc)
      return(conc)
      }  )
  df <- ldply(conc, data.frame)
  ifelse(nrow(df) >= 1, df <- df[,c(1, 3:ncol(df))], df <- df)
  colnames(df) <- c("OriginalString", "PreContext", "Token", "PostContext")
  rownames(df) <- 1:nrow(df)
  return(df)
}
###############################################################
### ---                    THE END
###############################################################
# Example
#strings <- pstggd[1:5]
#test <- ConcR(strings, pattern = "[A-Z]{0,1}[a-z]{1,}\\/JJ[A-Z]{0,1}", context = 30, all.pre = F)
# inspect results
#head(test)




