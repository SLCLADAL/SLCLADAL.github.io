################################################
# Editing the Corpus of Contemporary American English (COCA)
################################################
# Title: Editing the Corpus of Contemporary American English (COCA)
# Author: Martin Schweinberger
# Date: 2015-07-01
# Description: This script attaches the Part-of-Speech tags to the
# the words in the COCA.
################################################
#                      START
################################################
# Remove all lists from the current workspace
rm(list=ls(all=T))
# Install packages
#install.packages("stringr")
# Load packages
library(stringr)
################################################
# Setting options
options(stringsAsFactors = F)
#options(encoding = "UTF-8")
################################################
# write function
cocapostagging <- function(coca.files){
sapply(coca.files, function(x){
# extract names
  filename <- gsub(".*/", "", x)
  filename <- gsub(".txt", "", filename)
  x <- readLines(con = x, n = -1L, ok = TRUE, warn = TRUE, encoding = "unknown", skipNul = T)
# split data on \t
  x <- strsplit(x, "\t")
# select first three elements
  x <- sapply(x, function(y){ y <- y[c(1:3)] }) 
# convert into data.frames
  x <- t(x)
  x <- as.data.frame(x)
# paste words and pos tags together
  x <- apply(x, 1, function(y){
    y <- paste(y[1], "/", y[3], sep = "") })
# paste words together
  x <- paste(x, collapse = " ")
# add names
  names(x) <- filename
  writeLines(text = x, con = paste("C:\\Corpora\\edited\\COCApostagged/", names(x),".txt", sep = ""))
  #writeLines(text = x, con = paste("C:\\Corpora\\edited\\test/", names(x),".txt", sep = ""))
  rm(list=ls(all=T))
 })
}
################################################ 
# apply function to data
# defining paths to the corpus data - academic
path <- "C:\\Corpora\\original\\COCA\\Academic\\word lemma pos\\wlp_academic_rpe"
coca.files = list.files(path = path, pattern = NULL, all.files = T, full.names = T, recursive = T, ignore.case = T, include.dirs = T)
cocapostagging(coca.files = coca.files) #(remove # to to activate)
# defining paths to the corpus data - fiction
path <- "C:\\Corpora\\original\\COCA\\Fiction\\Word Lemma Pos\\wlp_fiction_awq"
coca.files = list.files(path = path, pattern = NULL, all.files = T, full.names = T, recursive = T, ignore.case = T, include.dirs = T)
cocapostagging(coca.files = coca.files) #(remove # to to activate)
# defining paths to the corpus data - magazine
path <- "C:\\Corpora\\original\\COCA\\Magazine\\Word Lemma Pos\\wlp_magazine_qgp"
coca.files = list.files(path = path, pattern = NULL, all.files = T, full.names = T, recursive = T, ignore.case = T, include.dirs = T)
cocapostagging(coca.files = coca.files) #(remove # to to activate)
# defining paths to the corpus data - newspaper
path <- "C:\\Corpora\\original\\COCA\\Newspaper\\Word Lemma Pos\\wlp_newspaper_lsp"
coca.files = list.files(path = path, pattern = NULL, all.files = T, full.names = T, recursive = T, ignore.case = T, include.dirs = T)
cocapostagging(coca.files = coca.files) #(remove # to to activate)
# defining paths to the corpus data - spoken
path <- "C:\\Corpora\\original\\COCA\\Spoken\\Word Lemma Pos\\wlp_spoken_kde"
coca.files = list.files(path = path, pattern = NULL, all.files = T, full.names = T, recursive = T, ignore.case = T, include.dirs = T)
cocapostagging(coca.files = coca.files) #(remove # to to activate)
