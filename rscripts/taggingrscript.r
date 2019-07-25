
# "POS-Tagging and Parsing"
# "UQ SLC Digital Team"
#
# clean current workspace
rm(list=ls(all=T))
# set options
options(stringsAsFactors = F)         # no automatic data transformation
options("scipen" = 100, "digits" = 4) # supress math annotation
# install libraries
install.packages(c("boot", "car", "effects", "ggplot2",
                   "Hmisc", "languageR", "lme4", "mlogit",
                   "msm", "plyr", "QuantPsyc", "RLRsim", "rms",
                   "sandwich", "sjPlot", "visreg"))
options(stringsAsFactors = FALSE)
library(tm)
library(NLP)
# read suto paragraphs
textdata <- read.csv("data/sotu_paragraphs.csv", sep = ";", encoding = "UTF-8")
english_stopwords <- readLines("resources/stopwords_en.txt", encoding = "UTF-8")
# Create corpus object
corpus <- Corpus(DataframeSource(textdata))
require(openNLP)
require(openNLPdata)
# openNLP annotator objects
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
pos_tag_annotator <- Maxent_POS_Tag_Annotator()
annotator_pipeline <- Annotator_Pipeline(
    sent_token_annotator,
    word_token_annotator,
    pos_tag_annotator
)
# function for annotation
annotateDocuments <- function(doc, pos_filter = NULL) {
  doc <- as.String(doc)
  doc_with_annotations <- annotate(doc, annotator_pipeline)
  tags <- sapply(subset(doc_with_annotations, type=="word")$features, `[[`, "POS")
  tokens <- doc[subset(doc_with_annotations, type=="word")]
  if (!is.null(pos_filter)) {
    res <- tokens[tags %in% pos_filter]
  } else {
    res <- paste0(tokens, "_", tags)
  }
  res <- paste(res, collapse = " ")
  return(res)
}
# run annotation on a sample of the corpus
annotated_corpus <- lapply(corpus[1:10], annotateDocuments)
# Have a look into the first annotated documents
annotated_corpus[[1]]
annotated_corpus[[2]]
sample_corpus <- sapply(corpus[1:1000], annotateDocuments, pos_filter = c("NNP", "NNPS"))
# Binary term matrix
require(Matrix)
minimumFrequency <- 2
filtered_corpus <- Corpus(VectorSource(sample_corpus))
binDTM <- DocumentTermMatrix(filtered_corpus, control=list(bounds = list(global=c(minimumFrequency, Inf)), weighting = weightBin))
# colnames(binDTM)
binDTM <- sparseMatrix(i = binDTM$i, j = binDTM$j, x = binDTM$v, dims = c(binDTM$nrow, binDTM$ncol), dimnames = dimnames(binDTM))
# Matrix multiplication for cooccurrence counts
coocCounts <- t(binDTM) %*% binDTM
source("rscripts/calculateCoocStatistics.R")
# Definition of a parameter for the representation of the co-occurrences of a concept
# Determination of the term of which co-competitors are to be measured.
coocTerm <- "indians"
coocs <- calculateCoocStatistics(coocTerm, binDTM, measure="LOGLIK")
print(coocs[1:20])r German, eval = F}
# install.packages("openNLPmodels.de", repos = "http://datacube.wu.ac.at")
# require("openNLPmodels.de")
