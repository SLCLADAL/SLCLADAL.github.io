##########################################################
### --- Part-of-Speech tagging with R
##########################################################
### --- R script "Part-of-Speech tagging with R"
### --- name: POStag.r
### --- author: Martin Schweinberger
### --- date: 2015-06-08
### --- description: This script adds POS tags to texts (a corpus).
### --- arguments: the function takes two arguments: 1. path and 2. language
### --- path: the path to directory in which the corpus files are stored and
### --- lang: the default is "english" which will cause "openNLPmodels.en"
### --- to be loaded. If lang is specified as "german", "openNLPmodels.de" is
### --- loaded.
##########################################################
POStag <- function(object = object){
  # load neccessary packages
  require("stringr")
  require("NLP")
  require("openNLP")
  require("openNLPmodels.en")
  # define paths to corpus files
  corpus.tmp <- object
  # paste content of each corpus files together
#  corpus.tmp <- lapply(corpus.tmp, function(x){
#    x <- paste(x, collapse = " ")  }  )
  # convert to utf8 encoding
#  corpus.tmp <- lapply(corpus.tmp, function(x) {
#    x <- enc2utf8(x)  }  )
  # get rid of superfluous spaces
#  corpus.tmp <- gsub(" {2,}", " ", corpus.tmp)
  # get rid of spaces at the beginning and end of texts
#  corpus.tmp <- str_trim(corpus.tmp, side = "both")
  # define sentence annotator
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  # define word annotator
  word_token_annotator <- Maxent_Word_Token_Annotator()
  # define pos annotator
  pos_tag_annotator <- Maxent_POS_Tag_Annotator(language = "en", probs = FALSE, 
    model = "C:\\Users\\marti\\OneDrive\\Dokumente\\R\\win-library\\3.4\\openNLPdata\\models\\en-pos-maxent.bin")
  # convert all file content to strings
  Corpus <- lapply(corpus.tmp, function(x){
    x <- as.String(x)  }  )
# loop over file contents
  lapply(Corpus, function(x){
#  lapply(object, function(x){
    y1 <- annotate(x, list(sent_token_annotator, word_token_annotator))
    y2<- annotate(x, pos_tag_annotator, y1)
#  y3 <- annotate(x, Maxent_POS_Tag_Annotator(probs = TRUE), y1)
    y2w <- subset(y2, type == "word")
    tags <- sapply(y2w$features, '[[', "POS")
    r1 <- sprintf("%s/%s", x[y2w], tags)
    r2 <- paste(r1, collapse = " ")
    return(r2)  }  )
  }

# test the function
#POStag(object = corpus.tmp, lang = "english")

