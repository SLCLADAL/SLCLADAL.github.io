# Text Analysis
# Introduction
# Preparation
# install libraries
install.packages(c("class", "cluster", "dplyr", "factoextra", 
"FactoMineR", "ggplot2", "ggraph", "grid", "gutenbergr", "igraph", 
"Matrix", "NLP", "openNLP", "openNLPmodels.en", "Matrix", "stringr", 
"syuzhet", "tidyr", "tidytext", "tm", "topicmodels", "wordcloud", "xtable"))
# Concordancing
# load libraries
library(dplyr)
library(stringr)
# read in text
darwin <- readLines("https://slcladal.github.io/data/origindarwin.txt") %>%
  paste(sep = " ", collapse = " ") %>%
  str_replace_all("(CHAPTER [XVI]{1,7}\\.{0,1}) ", "qwertz\\1") %>%
  tolower() %>%  
  strsplit("qwertz") %>%
  unlist()
# inspect data
nchar(darwin)

# load function for concordancing
source("https://slcladal.github.io/rscripts/ConcR_2.3_loadedfiles.r")
# start concordancing
darwinnatsel <- ConcR(darwin, "organism[s]{0,1}", 50)
# inspect data
darwinnatsel[1:5, 2:ncol(darwinnatsel)]

# clean data
darwinnatsel1 <- darwinnatsel[complete.cases(darwinnatsel),]
# determine chapter
darwinnatsel1$Chapter <- ifelse(grepl("chapter [xvi]{1,7}\\.{0,1} .*", darwinnatsel1$OriginalString) == T, gsub("(chapter [xvi]{1,7})\\.{0,1} .*", "\\1", darwinnatsel1$OriginalString), darwinnatsel1$OriginalString)
# remove OriginalString column 
darwinnatsel1$OriginalString <- NULL
# inspect data
head(darwinnatsel1)

# Word Frequency
# extract number of words per chapter
library(dplyr)
darwinchapters <- darwin %>%
  strsplit(" ")
words <- sapply(darwinchapters, function(x) length(x))
# inspect data
words

# extract number of matches per chapter
library(stringr)
matcheschapters <- darwin %>%
  str_extract_all(., "organism[s]{0,1}") 
matches <- sapply(matcheschapters, function(x) length(x))
# inspect data
matches

# extract chapters
Chapters <- as.vector(unlist(sapply(darwin, function(x){
  x <- gsub("(chapter [xvi]{1,7})\\.{0,1} .*", "\\1", x)
  x <- ifelse(nchar(x) > 50, "chapter 0", x)
})))
# calculate rel. freq of serach term per chapter 
Frequency <- matches/words*1000
# create table of results
tb <- data.frame(Chapters, Frequency)
# inspect results
head(tb)

# load library
library(ggplot2)
# create plot
ggplot(tb, aes(x=Chapters, y=Frequency, group =1)) + 
  geom_smooth(aes(y = Frequency, x = Chapters), color = "goldenrod2")+
  geom_line(aes(y = Frequency, x = Chapters), color = "indianred4") +         
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(name ="Relative Frequency (per 1,000 words)")

# load library
library(tm)
# load and process corpus
corpuswords <- readLines("https://slcladal.github.io/data/origindarwin.txt")  %>%
  tolower() %>%
  removeWords(stopwords("english")) %>% 
  str_replace_all("[^[:alpha:][:space:]]*", "")  %>%
  paste(sep = " ", collapse = " ") %>%
  str_replace_all(" {2,}", " ") %>%
  strsplit(" ") %>%
  unlist()  
# create table
wordfreqs <- corpuswords %>%
  table() %>%
  as.data.frame() %>%
  arrange(desc(Freq))
# add column names
colnames(wordfreqs) <- c("Word", "Frequency")
# inspect data
head(wordfreqs)

# prepare data
wfd <- table(corpuswords)
wfd <- wfd[order(wfd, decreasing = T)]
wfd <- wfd[1:10]
# start plot
barplot(wfd, las = 1, ylim = c(0,2000), las=2)
text(seq(0.7, 11.5, 1.2), wfd+150, wfd)

# load library
library("wordcloud")
# create wordcloud
wordcloud(words = wordfreqs$Word, freq = wordfreqs$Frequency, 
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "BrBG"))

# load data
orwell <- readLines("https://slcladal.github.io/data/orwell.txt")
melville <- readLines("https://slcladal.github.io/data/melvillemobydick.txt")
# combine each text into one element
orwell <- paste(as.vector(unlist(orwell)), sep = " ", collapse = " ")
melville <- paste(as.vector(unlist(melville)), sep = " ", collapse = " ")
# load libraries
library(tm)
library(dplyr)
library(xtable)
# clean texts
docs <- Corpus(VectorSource(c(orwell, melville))) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(tolower)  %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stripWhitespace) %>%
  tm_map(PlainTextDocument)
# create term document matrix
tdm <- TermDocumentMatrix(docs) %>%
  as.matrix()
colnames(tdm) <- c("Orwell","Melville")
# create comparison cloud
comparison.cloud(tdm, random.order=FALSE, 
                 colors = c("orange","lightblue"),
                 title.size=2.5, max.words=200, 
                 title.bg.colors = "white")

d <- matrix(c(6761, 659, 259625, 105295), nrow = 2, byrow = T)
colnames(d) <- c("D", "M")
rownames(d) <- c("you, your", "Other words")
assocplot(d)

# Collocations and N-grams
# load libraries
library(dplyr)
library(stringr)
library(tm)
# read in text
darwin <- readLines("https://slcladal.github.io/data/origindarwin.txt") %>%
  paste(sep = " ", collapse = " ") %>%
  removePunctuation() %>%
  str_replace_all(" {2,}", " ") %>% 
  tolower() %>%
  strsplit(" ") %>%
  unlist()
# inspect data
head(darwin)

# create data frame
darwindf <- data.frame(darwin[1:length(darwin)-1], 
                       darwin[2:length(darwin)])
# add column names
colnames(darwindf) <- c("Word1", "Word2")
# inspect data
head(darwindf)

# create data frame
darwin2grams <- paste(darwindf$Word1, darwindf$Word2, sep = " ")
# tabulate results
darwin2gramstb <- table(darwin2grams)
# create data frame
darwin2gramsdf <- data.frame(darwin2gramstb)
# order data frame
darwin2gramsdf <- darwin2gramsdf[order(darwin2gramsdf$Freq, decreasing = T),]
# simplify column names
colnames(darwin2gramsdf) <- c("Bigram", "Frequency")
# inspect data
head(darwin2gramsdf)

# read in text
darwinsentences <- readLines("https://slcladal.github.io/data/origindarwin.txt") %>%
  paste(sep = " ", collapse = " ") %>%
  str_replace_all(" {2,}", " ") %>%
  str_replace_all("([A-Z]{2,} [A-Z]{2,}) ([A-Z][a-z]{1,} )", "\\1 qwertz\\2") %>%
  str_replace_all("([a-z]{2,}\\.) ([A-Z] {0,1}[a-z]{0,30})", "\\1qwertz\\2") %>%
    str_replace_all("([a-z]{2,}\\?) ([A-Z] {0,1}[a-z]{0,30})", "\\1qwertz\\2") %>%
  strsplit("qwertz")%>%
  unlist()
# inspect data
head(darwinsentences)

# convert into corpus
darwincorpus <- Corpus(VectorSource(darwinsentences))
# create vector with words to remove
extrawords <- c("the", "can", "get", "got", "can", "one", 
                "dont", "even", "may", "but", "will", 
                "much", "first", "but", "see", "new", 
                "many", "less", "now", "well", "like", 
                "often", "every", "said", "two")
# clean corpus
darwincorpusclean <- darwincorpus %>%
  tm_map(removePunctuation) %>%
  tm_map(tolower) %>%
  tm_map(removeWords, stopwords(kind = "en")) %>%
  tm_map(removeWords, extrawords)
# create document term matrix
darwindtm <- DocumentTermMatrix(darwincorpusclean, control=list(bounds = list(global=c(1, Inf)), weighting = weightBin))
# load library
require(Matrix)
# convert dtm into sparse matrix
darwinsdtm <- sparseMatrix(i = darwindtm$i, j = darwindtm$j, 
                           x = darwindtm$v, 
                           dims = c(darwindtm$nrow, darwindtm$ncol),
                           dimnames = dimnames(darwindtm))
# calculate cooccurrence counts
coocurrences <- t(darwinsdtm) %*% darwinsdtm
# convert into matrix
collocates <- as.matrix(coocurrences)
# inspect results
collocates[1:8, 1:5]

# inspect size of matrix
ncol(collocates)
summary(rowSums(collocates))

# remove terms that do not collocate with other terms
noncoll <- colnames(collocates)[which(rowSums(collocates) < 5000)]
# remove non-collocating terms
collocates <- collocates[!rownames(collocates) %in% noncoll, ]
collocates <- collocates[, !colnames(collocates) %in% noncoll]
# create distance matrix
distmtx <- dist(collocates)
# activate library
library("cluster")         # activate library
clustertexts <- hclust(    # hierarchical cluster object
  distmtx,                 # use data diststudents
  method="ward.D2")         # ward.D as linkage method
plot(clustertexts,         # plot result as dendrogram
     hang = .25,           # labels at split
     main = "")            # no title

# load library
library("FactoMineR")
library("factoextra")
# perform correspondence analysis
res.ca <- CA(collocates, graph = FALSE)
# cerate bi-plot
#fviz_ca_biplot(res.ca, repel = F, select.ind = list(cos2 = .02))
plot(res.ca, shadow = T, cex = 1, selectRow = "cos2 0.1", selectCol = "cos2 0.9", col.row = "gray50", title = "")

# load function for co-occurrence calculation
source("rscripts/calculateCoocStatistics.R")
# define minimum number of cooccurences
numberOfCoocs <- 10
# define term
coocTerm <- "selection"
# calculate cooccurence statistics
coocs <- calculateCoocStatistics(coocTerm, darwinsdtm, measure="LOGLIK")
# show strenght of cooccurence
print(coocs[1:numberOfCoocs])

# create graph object
resultGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
# create data frame
tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
# fill data frame to produce the correct number of lines
tmpGraph[1:numberOfCoocs, 3] <- coocs[1:numberOfCoocs]
# enter search word into the first column in all lines
tmpGraph[, 1] <- coocTerm
# enter co-occurrences into second column
tmpGraph[, 2] <- names(coocs)[1:numberOfCoocs]
# enter collocation strength
tmpGraph[, 3] <- coocs[1:numberOfCoocs]
# attach data frame to resultGraph
resultGraph <- rbind(resultGraph, tmpGraph)

# iterate over most significant numberOfCoocs co-occurrences
for (i in 1:numberOfCoocs){
  # calculate co-occurrence strength for term i
  newCoocTerm <- names(coocs)[i]
  coocs2 <- calculateCoocStatistics(newCoocTerm, darwinsdtm, measure="LOGLIK")
  # fill temporary graph object
  tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
  tmpGraph[1:numberOfCoocs, 3] <- coocs2[1:numberOfCoocs]
  tmpGraph[, 1] <- newCoocTerm
  tmpGraph[, 2] <- names(coocs2)[1:numberOfCoocs]
  tmpGraph[, 3] <- coocs2[1:numberOfCoocs]
  # append results to the result graph data frame
  resultGraph <- rbind(resultGraph, tmpGraph[2:length(tmpGraph[, 1]), ])
}

# load packages
library(igraph)
# define graph and type ("F" means "Force Directed")
graphNetwork <- graph.data.frame(resultGraph, directed = F)
# identify nodes with fewer than 2 edges
graphVs <- V(graphNetwork)[degree(graphNetwork) < 2]
# removed these edges from graph
graphNetwork <- delete.vertices(graphNetwork, graphVs) 
# sssign colors to edges and nodes (searchterm blue, rest orange)
V(graphNetwork)$color <- ifelse(V(graphNetwork)$name == coocTerm, 'cornflowerblue', 'orange') 
# Edges with a significance of at least 50% of the maximum significance in the graph are drawn in orange
halfMaxSig <- max(E(graphNetwork)$sig) * 0.5
E(graphNetwork)$color <- ifelse(E(graphNetwork)$sig > halfMaxSig, "coral", "azure3")
# disable edges with radius
E(graphNetwork)$curved <- 0 
# size the nodes by their degree of networking
V(graphNetwork)$size <- log(degree(graphNetwork)) * 5
# all nodes must be assigned a standard minimum-size
V(graphNetwork)$size[V(graphNetwork)$size < 5] <- 3 
# edge thickness
E(graphNetwork)$width <- 1.5

# Define the frame and spacing for the plot
par(mai=c(0,0,1,0)) 
# Finaler Plot
plot(graphNetwork,              
     layout = layout.fruchterman.reingold,  # Force Directed Layout 
     main = paste("Cooccurrence network for", " \"", coocTerm, "\""),
     vertex.label.family = "sans",
     vertex.label.cex = .75,
     vertex.shape = "circle",
     vertex.label.dist = 2,           # Labels of the nodes moved slightly
     vertex.frame.color = 'darkolivegreen',
     vertex.label.color = 'black',      # Color of node names
     vertex.label.font = 2,         # Font of node names
     vertex.label = V(graphNetwork)$name,       # node names
     vertex.label.cex = .75 # font size of node names 
)


# Tagging and Annotation
## Part-of-speech tagging (pos tagging)
# load corpus data
text <- readLines("https://slcladal.github.io/data/text2.txt", skipNul = T)
# clean data
text <- text[5] %>%
  removeNumbers() %>%
  stripWhitespace() %>%
  str_replace_all("\"", "")  %>%
  str_replace_all("When Harry.*", "")  %>%
  strsplit("qwertz") %>%
  unlist() %>%
  stripWhitespace() 
# inspect data
str(text)

# load function
source("https://slcladal.github.io/rscripts/POStagObject.r") # for pos-tagging objects in R
# load libraries
library(NLP)
library(openNLP)
library(openNLPmodels.en)
# detach ggplot2 library becuase function "annotate" 
# would be taken from ggplot2 rather than NLP
detach("package:factoextra", unload=TRUE)
detach("package:ggplot2", unload=TRUE)
# pos tagging data
textpos <- POStag(object = text)
textpos

## Syntactic Parsing
# extract text
text <- gsub("He inquires.*", "", text)
# convert character to string
s <- as.String(text)
# define sentence and word token annotator
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
# apply sentence and word annotatior
a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))
# define syntactic parsing annotator
parse_annotator <- Parse_Annotator()
# apply parser
p <- parse_annotator(s, a2)
# extract parsed information
ptexts <- sapply(p$features, '[[', "parse")
ptexts

# read into NLP Tree objects.
ptrees <- lapply(ptexts, Tree_parse)
# show frist tree
ptrees[[1]]

# remove punctuation
ptexts[1] <- gsub("\\(\\. \\.\\)", "", ptexts[1])
ptexts[1] <- gsub("\\(\\, \\,\\)", "", ptexts[1])
# load library
library(igraph)
source("https://slcladal.github.io/rscripts/parse2graph.r")
parse2graph(ptexts[1], title = "", margin=-0.2, vertex.color=NA,
            vertex.frame.color=NA, vertex.label.font=2,
            vertex.label.cex=.75, vertex.label.color="black", asp=.5,
            edge.width=1, edge.color='red', edge.arrow.size=0)

# Text Classification
# read in German text
German <- readLines("https://slcladal.github.io/data/phonemictext1.txt")
# clean text
German <- gsub(" ", "", German)
# split text into phonemes
German <- strsplit(German, "")
# unlist and convert into vector
German <- as.vector(unlist(German))
# inspect data
head(German)

# read in texts
English <- readLines("https://slcladal.github.io/data/phonemictext2.txt")
Spanish <- readLines("https://slcladal.github.io/data/phonemictext3.txt")
Unknown <- readLines("https://slcladal.github.io/data/phonemictext4.txt")
# clean, split texts into phonemes, unlist and convert them into vectors
English <- as.vector(unlist(strsplit(gsub(" ", "", English), "")))
Spanish <- as.vector(unlist(strsplit(gsub(" ", "", Spanish), "")))
Unknown <- as.vector(unlist(strsplit(gsub(" ", "", Unknown), "")))
# inspect data
head(English, 10)

# create data tables
German <- data.frame(names(table(German)), as.vector(table(German)))
English <- data.frame(names(table(English)), as.vector(table(English)))
Spanish <- data.frame(names(table(Spanish)), as.vector(table(Spanish)))
Unknown <- data.frame(names(table(Unknown)), as.vector(table(Unknown)))
# add column with language
German$Language <- "German"
English$Language <- "English"
Spanish$Language <- "Spanish"
Unknown$Language <- "Unknown"
# simlify column names
colnames(German)[1:2] <- c("Phoneme", "Frequency")
colnames(English)[1:2] <- c("Phoneme", "Frequency")
colnames(Spanish)[1:2] <- c("Phoneme", "Frequency")
colnames(Unknown)[1:2] <- c("Phoneme", "Frequency")
# comine all tables into a single table
classdata <- rbind(German, English, Spanish, Unknown) 
# inspect table for English
head(classdata)

# set options
options(stringsAsFactors = F)
# create wide format
classdatanew <- reshape(classdata, idvar = "Language", timevar = "Phoneme",direction = "wide")
classdw <- t(apply(classdatanew, 1, function(x){ 
  x <- ifelse(is.na(x) == T, 0, x)}))
# simplify column names
colnames(classdw) <- gsub("Frequency.", "", colnames(classdw))
# convert into data frame
classdw <- as.data.frame(classdw)
# inspect data
classdw[, 1:6]

numvar <- colnames(classdw)[2:length(colnames(classdw))]
classdw[numvar] <- lapply(classdw[numvar], as.numeric)
# function for normalizing numeric variables
normalize <- function(x) { (x -min(x))/(max(x)-min(x))   }
# apply normalization
 classdw[numvar] <- as.data.frame(lapply(classdw[numvar], normalize))
 # inspect data
classdw[, 1:5]

# remove language column
textm <- classdw[,2:ncol(classdw)]
# add languages as row names
rownames(textm) <- classdw[,1]
# create distance matrix
distmtx <- dist(textm)
# activate library
library("cluster")         # activate library
clustertexts <- hclust(    # hierarchical cluster object
  distmtx,                 # use data diststudents
  method="ward.D")         # ward.D as linkage method
plot(clustertexts,         # plot result as dendrogram
     hang = .25,           # labels at split
     main = "")            # no title

#load library
library(dplyr)
# create training set
train <- classdw %>%
  filter(Language != "Unknown")
# increase training set size
train <- rbind(train, train, train, train, train, train, train, train)
# create test set
test <- classdw %>%
  filter(Language == "Unknown")
# convert variables
train$Language <- as.factor(train$Language)
train$Language <- as.factor(train$Language)
# inspect data
train[1:10, 1:3]; test[, 1:3]

# activate library
library("class")
# apply k-nearest-neighbor (knn) classifier
prediction <- class::knn(train[,2:ncol(train)], test[,2:ncol(test)], cl = train[, 1], k = 3)
# inspect the result
prediction

# Sentiment Analysis
# read in texts
darwin <- readLines("https://slcladal.github.io/data/origindarwin.txt")
twain <- readLines("https://slcladal.github.io/data/twainhuckfinn.txt")
orwell <- readLines("https://slcladal.github.io/data/orwell.txt")
lovecraft <- readLines("https://slcladal.github.io/data/lovecraftcolor.txt")
husband <- readLines("https://slcladal.github.io/data/husbandsregret.txt")
# clean and split files into words 
darwin <- tolower(as.vector(unlist(strsplit(paste(gsub(" {2,}", " ", darwin), sep = " "), " "))))
twain <- tolower(as.vector(unlist(strsplit(paste(gsub(" {2,}", " ", twain), sep = " "), " "))))
orwell <- tolower(as.vector(unlist(strsplit(paste(gsub(" {2,}", " ", orwell), sep = " "), " "))))
lovecraft <- tolower(as.vector(unlist(strsplit(paste(gsub(" {2,}", " ", lovecraft), sep = " "), " "))))
husband <- tolower(as.vector(unlist(strsplit(paste(gsub(" {2,}", " ", husband), sep = " "), " "))))
darwin <- sample(darwin, 5000, replace = F)
twain <- sample(twain, 5000, replace = F)
orwell <- sample(orwell, 5000, replace = F)
lovecraft <- sample(lovecraft, 5000, replace = F)
husband <- sample(husband, 5000, replace = F)
# load library
library(syuzhet)
# perform sentiment analysis
darwinemo <- get_nrc_sentiment(darwin)
twainemo <- get_nrc_sentiment(twain)
orwellemo <- get_nrc_sentiment(orwell)
lovecraftemo <- get_nrc_sentiment(lovecraft)
husbandemo <- get_nrc_sentiment(husband)
# inspect data
head(darwinemo)

# extract percentages of emotional words
darwinemos <- colSums(darwinemo)/50
twainemos <- colSums(twainemo)/50
orwellemos <- colSums(orwellemo)/50
lovecraftemos <- colSums(lovecraftemo)/50
husbandemos <- colSums(husbandemo)/50
# collapse into a single table
emolit <- data.frame(darwinemos, twainemos, orwellemos, lovecraftemos, husbandemos)
# transpose data
emo <- t(emolit)
# clean row names
rownames(emo) <- gsub("emos", "", rownames(emo))
# inspect data
head(emo)

#convert into data frame
emo <- as.data.frame(emo)
# add author column
emo$Author <- c("Darwin", "Twain", "Orwell", "Lovecraft", "Husband")
# load library
library(tidyr)
# convert data from wide to long
emol <- gather(emo, Emotion, Score, anger:positive, factor_key=TRUE)
# inspect data
head(emol)

# load library
library(ggplot2)
# extract subset
emol2 <- emol %>%
  filter(Emotion != "positive") %>%
  filter(Emotion != "negative")
# start plot
ggplot(emol2,                   # plot barplotdatagg1
       aes(Emotion, Score,      # define x- and y-axis
           fill = Author)) +    # define grouping variable
  geom_bar(stat="identity",     # determine type of plot
           position=position_dodge()) +  # determine grouping
  scale_fill_manual(values=c("goldenrod2", "gray70", "indianred4", "grey30", "lightblue")) +                 # define colours
  theme_bw()                    # define theme (black and white)

# Entity Extraction
# load libraries
library(NLP)
library(openNLP)
library(openNLPmodels.en)
# load text
orwell <- readLines("https://slcladal.github.io/data/orwell.txt")
orwell <- orwell  %>%
  paste(sep = " ", collapse = " ") %>%
  str_replace_all(" {2,}", " ") %>%
  str_replace_all("Part 2,.*", "")
# convert text into string
orwell = as.String(orwell)
# define annotators
sent_annot = Maxent_Sent_Token_Annotator()
word_annot = Maxent_Word_Token_Annotator()
loc_annot = Maxent_Entity_Annotator(kind = "location") 
people_annot = Maxent_Entity_Annotator(kind = "person") 
# start annotation
orwellanno = NLP::annotate(orwell, list(sent_annot, word_annot, 
                                        loc_annot, people_annot))
# extract features
k <- sapply(orwellanno$features, `[[`, "kind")
# extract locations
orwelllocations = names(table(orwell[orwellanno[k == "location"]]))
# extract people
orwellpeople = names(table(orwell[orwellanno[k == "person"]]))
# inspect extract people
orwellpeople

# Topic Modelling
# load data
darwin <- readLines("https://slcladal.github.io/data/origindarwin.txt")
twain <- readLines("https://slcladal.github.io/data/twainhuckfinn.txt")
orwell <- readLines("https://slcladal.github.io/data/orwell.txt")
# clean files 
darwin <- paste(gsub(" {2,}", " ", darwin), sep = " ", collapse = " ")
twain <- paste(gsub(" {2,}", "  ", twain), sep = " ", collapse = " ")
orwell <- paste(gsub(" {2,}", " ", orwell), sep = " ", collapse = " ")
# inspect data
str(orwell)

# load library
library(tm)
# create corpus object
texts <- Corpus(VectorSource(c(darwin, twain, orwell)))
# create vector with words to remove
extrawords <- c("the", "can", "get", "got", "can", "one", 
                "dont", "even", "may", "but", "will", 
                "much", "first", "but", "see", "new", 
                "many", "less", "now", "well", "like", 
                "often", "every", "said", "two")
# load libraries
library(dplyr)
library(stringr)
# clean corpus
textsclean <- texts %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>% 
  tm_map(tolower) %>%
  tm_map(removeWords, stopwords(kind = "en")) %>%
  tm_map(removeWords, extrawords)
# create DTM
textsdtm <- DocumentTermMatrix(textsclean)
# load library
library(topicmodels)
# perform LDA
textslda <- LDA(textsdtm, k = 3, control = list(seed = 20190712))
textslda

# load library
library(tidytext)
# convert data into tidy format
textstopics <- tidy(textslda, matrix = "beta")
textstopics

# load libraries
library(ggplot2)
library(dplyr)
# extract top words
textstopicstop <- textstopics %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(term, -beta)
# create plots
textstopicstop %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# Network Analysis
# load libraries
library(gutenbergr)
library(dplyr)
# load data
romeo <- gutenberg_works(title == "Romeo and Juliet") %>%
  gutenberg_download(meta_fields = "title")
# inspect data
romeo

# load library
library(stringr)
# load split
romeoscenes <- romeo %>%
  select(text) %>%
  as.vector() %>%
  str_replace_all(fixed("\""), "") %>%
  str_replace_all(fixed("\n"), "") %>%
  paste(collapse = " ") %>%
  str_replace_all("(Scene )", "qwertz\\1") %>%
  strsplit("qwertz") %>%
  unlist()
# inspect data
str(romeoscenes[2])

# load library
library(stringr)
# extract persons
romeopersonas <- romeoscenes %>%
  str_match_all(" , [A-Z][a-z]{2,} {0,1}[A-Z]{0,1}[a-z]{0,}\\.")
# inspect data
str(romeopersonas[1:5])

# extract personas per scene
personas <- sapply(romeopersonas, function(x){
  x <- unlist(x)
  x <- gsub(",", "", x)
  x <- gsub("\\.", "", x)
  x <- gsub(" ", "", x)
  x <- unique(x)
  x <- as.vector(x)
  x <- paste(x, collapse = " ")
  x <- gsub(" ActV", "", x)
  x <- gsub(" Page", "", x)
})
# inspect data
personas

# remove first elements
personas <- personas[2:length(personas)]
str(personas)

# load library
library(tm)
# create corpus
corpus <- Corpus(VectorSource(personas))
# create document term matrix
scenepersonas <- DocumentTermMatrix(corpus) 
# load library
require(Matrix)
# convert dtm into sparse matrix
rnjdtm <- sparseMatrix(i = scenepersonas$i, j = scenepersonas$j, 
                           x = scenepersonas$v, 
                           dims = c(scenepersonas$nrow,
                                    scenepersonas$ncol),
                           dimnames = dimnames(scenepersonas))
# calculate cooccurrence counts
coocurrence <- t(rnjdtm) %*% rnjdtm
# convert into matrix
romeom <- as.matrix(coocurrence)
# inspect data
head(romeom)

# create cooccurence table
persona1 <- rep(colnames(romeom), each = nrow(romeom))
persona2 <- rep(rownames(romeom), ncol(romeom))
freq <- as.vector(unlist(romeom))
# combine into data frame
dc <- data.frame(persona1, persona2, freq)
# remove cooccurence with oneself
df <- dc %>%
  filter(persona1 != persona2)
# inspect data
head(df)

# load library 
library(igraph)
# remove infrequent cooccurences and create graph object
bigram_graph <- df %>%
  graph_from_data_frame()
# load library
library(ggraph)
library(grid)
# define arrow type
a <- grid::arrow(type = "open", length = unit(.15, "cm"))
# create plot
ggraph(bigram_graph, layout = "auto") +
  geom_edge_link(aes(edge_alpha = freq), show.legend = FALSE,
                 arrow = a, end_cap = circle(1, 'inches')) +
  geom_node_point(color = "lightblue", size = 3) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
