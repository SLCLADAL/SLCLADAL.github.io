
# "Topic Models"
# "UQ SLC Digital Team"
#
# setwd("Your work directory")
options(stringsAsFactors = FALSE)
library(tm)
require(topicmodels)
textdata <- read.csv("data/sotu_paragraphs.csv", sep = ";", encoding = "UTF-8")
english_stopwords <- readLines("resources/stopwords_en.txt", encoding = "UTF-8")
# Create corpus object
corpus <- Corpus(DataframeSource(textdata))
# Preprocessing chain
processedCorpus <- tm_map(corpus, content_transformer(tolower))
processedCorpus <- tm_map(processedCorpus, removeWords, english_stopwords)
processedCorpus <- tm_map(processedCorpus, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus <- tm_map(processedCorpus, removeNumbers)
processedCorpus <- tm_map(processedCorpus, stemDocument, language = "en")
processedCorpus <- tm_map(processedCorpus, stripWhitespace)r}
# have a look a some of the results (posterior distributions)
tmResult <- posterior(topicModel)
# format of the resulting object
attributes(tmResult)
nTerms(DTM)              # lengthOfVocab
# topics are probability distribtions over the entire vocabulary
beta <- tmResult$terms   # get beta from results
dim(beta)                # K distributions over nTerms(DTM) terms
rowSums(beta)            # rows in beta sum to 1
nDocs(DTM)               # size of collection
# for every document we have a probaility distribution of its contained topics
theta <- tmResult$topics 
dim(theta)               # nDocs(DTM) distributions over K topics
rowSums(theta)[1:10]     # rows in theta sum to 1r}
# see alpha from previous model
attr(topicModel, "alpha") r, results='hide'}
# re-rank top topic terms for topic names
topicNames <- apply(lda::top.topic.words(beta, 5, by.score = T), 2, paste, collapse = " ")r, echo=FALSE}
soP <- sort(topicProportions, decreasing = TRUE)
paste(round(soP, 5), ":", names(soP))r, echo=FALSE}
so <- sort(countsOfPrimaryTopics, decreasing = TRUE)
paste(so, ":", names(so))
# append decade information for aggregation
textdata$decade <- paste0(substr(textdata$date, 0, 3), "0")
# get mean topic proportions per decade
topic_proportion_per_decade <- aggregate(theta, by = list(decade = textdata$decade), mean)
# set topic names to aggregated columns
colnames(topic_proportion_per_decade)[2:(K+1)] <- topicNames
# reshape data frame
vizDataFrame <- melt(topic_proportion_per_decade, id.vars = "decade")
# plot topic proportions per deacde as bar plot
require(pals)
ggplot(vizDataFrame, aes(x=decade, y=value, fill=variable)) + 
  geom_bar(stat = "identity") + ylab("proportion") + 
  scale_fill_manual(values = paste0(alphabet(20), "FF"), name = "decade") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
The visualization shows that topics around the relation between the federal government and the states as well as inner conflicts clearly dominate the first decades. Security issues and the economy are the most important topics of recent SOTU addresses.
# Optional exercises
1. Create a list of all documents that have a thematic context with *economy*.
2. Repeat the exercises with a different K value (e.g., K = 5, 30, 50). What do you think of the results?
3. Split the original text source into paragraphs (e.g. use `strsplit(textdata$text, "\n\n")`) and compute a topic model on paragraphs instead of full speeches. How does the smaller context unit affect the result?
# References
