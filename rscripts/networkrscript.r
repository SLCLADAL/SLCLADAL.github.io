
# "Network Analysis"
# "UQ SLC Digital Team"
#
options(stringsAsFactors = FALSE)
library(tm)
textdata <- read.csv("data/sotu.csv", sep = ";", encoding = "UTF-8")
english_stopwords <- readLines("resources/stopwords_en.txt", encoding = "UTF-8")
# Create corpus object
corpus <- Corpus(DataframeSource(textdata))
require(openNLP)
# Function to convert a document in a vector of sentences
convert_text_to_sentences <- function(text, lang = "en", SentModel = "resources/en-sent.bin") {
  # Calculate sentenve boundaries as annotation with Apache OpenNLP Maxent-sentence-detector.
  sentence_token_annotator <- Maxent_Sent_Token_Annotator(language = lang, model = SentModel)
  # Convert text to NLP string
  text <- NLP::as.String(text)
  # Annotate the sentence boundaries
  sentenceBoundaries <- NLP::annotate(text, sentence_token_annotator)
  # Select sentences as rows of a new matrix
  sentences <- text[sentenceBoundaries]
  # return the sentences
  return(sentences)
}
# Function to convert a corpus of documents into a corpus of single sentences from documents
reshape_corpus <- function(currentCorpus, ...) {
  # Extraction of all sentences from the corpus as a list
  text <- lapply(currentCorpus, as.character)
  # convert the text
  pb <- txtProgressBar(min=0, max=length(text))
  i <- 0
  docs <- lapply(text, FUN=function(x){
    i <<- i + 1
    setTxtProgressBar(pb, i)
    convert_text_to_sentences(x)
  }, ...)
  close(pb)
  docs <- as.vector(unlist(docs))
  # Create a new corpus of the segmented sentences
  newCorpus <- Corpus(VectorSource(docs))
  return(newCorpus)
}r reshapeCorpus, cache=TRUE}
# original corpus length and its first document
length(corpus)
substr(as.character(corpus[[1]]), 0, 200)
# reshape into sentences
sentenceCorpus <- reshape_corpus(corpus)
# reshaped corpus length and its first 'document'
length(sentenceCorpus)
as.character(sentenceCorpus[[1]])
as.character(sentenceCorpus[[2]])r preprocessCorpus, cache=TRUE}
# Preprocessing chain
sentenceCorpus <- tm_map(sentenceCorpus, content_transformer(tolower))
sentenceCorpus <- tm_map(sentenceCorpus, removeWords, english_stopwords)
sentenceCorpus <- tm_map(sentenceCorpus, removePunctuation, preserve_intra_word_dashes = TRUE)
sentenceCorpus <- tm_map(sentenceCorpus, removeNumbers)
sentenceCorpus <- tm_map(sentenceCorpus, stripWhitespace)r binDTM, cache=TRUE}
minimumFrequency <- 10
binDTM <- DocumentTermMatrix(sentenceCorpus, control=list(bounds = list(global=c(minimumFrequency, Inf)), weighting = weightBin))r message=FALSE}
# Convert to sparseMatrix matrix
require(Matrix)
binDTM <- sparseMatrix(i = binDTM$i, j = binDTM$j, x = binDTM$v, dims = c(binDTM$nrow, binDTM$ncol), dimnames = dimnames(binDTM))
# Matrix multiplication for cooccurrence counts
coocCounts <- t(binDTM) %*% binDTMr}
as.matrix(coocCounts[202:205, 202:205])r}
coocTerm <- "spain"
k <- nrow(binDTM)
ki <- sum(binDTM[, coocTerm])
kj <- colSums(binDTM)
names(kj) <- colnames(binDTM)
kij <- coocCounts[coocTerm, ]
########## MI: log(k*kij / (ki * kj) ########
mutualInformationSig <- log(k * kij / (ki * kj))
mutualInformationSig <- mutualInformationSig[order(mutualInformationSig, decreasing = TRUE)]
########## DICE: 2 X&Y / X + Y ##############
dicesig <- 2 * kij / (ki + kj)
dicesig <- dicesig[order(dicesig, decreasing=TRUE)]
########## Log Likelihood ###################
logsig <- 2 * ((k * log(k)) - (ki * log(ki)) - (kj * log(kj)) + (kij * log(kij)) 
          + (k - ki - kj + kij) * log(k - ki - kj + kij) 
          + (ki - kij) * log(ki - kij) + (kj - kij) * log(kj - kij) 
          - (k - ki) * log(k - ki) - (k - kj) * log(k - kj))
logsig <- logsig[order(logsig, decreasing=T)]r}
# Put all significance statistics in one Data-Frame
resultOverView <- data.frame(
  names(sort(kij, decreasing=T)[1:10]), sort(kij, decreasing=T)[1:10],
  names(mutualInformationSig[1:10]), mutualInformationSig[1:10], 
  names(dicesig[1:10]), dicesig[1:10], 
  names(logsig[1:10]), logsig[1:10],
  row.names = NULL)
colnames(resultOverView) <- c("Freq-terms", "Freq", "MI-terms", "MI", "Dice-Terms", "Dice", "LL-Terms", "LL")
print(resultOverView)r}
# Read in the source code for the co-occurrence calculation
source("rscripts/calculateCoocStatistics.R")
# Definition of a parameter for the representation of the co-occurrences of a concept
numberOfCoocs <- 15
# Determination of the term of which co-competitors are to be measured.
coocTerm <- "california"r}
coocs <- calculateCoocStatistics(coocTerm, binDTM, measure="LOGLIK")
# Display the numberOfCoocs main terms
print(coocs[1:numberOfCoocs])
resultGraph <- data.frame(from = character(), to = character(), sig = numeric(0))r}
# The structure of the temporary graph object is equal to that of the resultGraph
tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
# Fill the data.frame to produce the correct number of lines
tmpGraph[1:numberOfCoocs, 3] <- coocs[1:numberOfCoocs]
# Entry of the search word into the first column in all lines
tmpGraph[, 1] <- coocTerm
# Entry of the co-occurrences into the second column of the respective line
tmpGraph[, 2] <- names(coocs)[1:numberOfCoocs]
# Set the significances
tmpGraph[, 3] <- coocs[1:numberOfCoocs]
# Attach the triples to resultGraph
resultGraph <- rbind(resultGraph, tmpGraph)
# Iteration over the most significant numberOfCoocs co-occurrences of the search term
for (i in 1:numberOfCoocs){
  # Calling up the co-occurrence calculation for term i from the search words co-occurrences
  newCoocTerm <- names(coocs)[i]
  coocs2 <- calculateCoocStatistics(newCoocTerm, binDTM, measure="LOGLIK")
  #print the co-occurrences
  coocs2[1:10]
  # Structure of the temporary graph object
  tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
  tmpGraph[1:numberOfCoocs, 3] <- coocs2[1:numberOfCoocs]
  tmpGraph[, 1] <- newCoocTerm
  tmpGraph[, 2] <- names(coocs2)[1:numberOfCoocs]
  tmpGraph[, 3] <- coocs2[1:numberOfCoocs]
  #Append the result to the result graph
  resultGraph <- rbind(resultGraph, tmpGraph[2:length(tmpGraph[, 1]), ])
}r}
# Sample of some examples from resultGraph
resultGraph[sample(nrow(resultGraph), 6), ]r fig.width=10, fig.height=8, message=FALSE}
require(igraph)
# Set the graph and type. In this case, "F" means "Force Directed"
graphNetwork <- graph.data.frame(resultGraph, directed = F)
# Identification of all nodes with less than 2 edges
graphVs <- V(graphNetwork)[degree(graphNetwork) < 2]
# These edges are removed from the graph
graphNetwork <- delete.vertices(graphNetwork, graphVs) 
# Assign colors to edges and nodes (searchterm blue, rest orange)
V(graphNetwork)$color <- ifelse(V(graphNetwork)$name == coocTerm, 'cornflowerblue', 'orange') 
# Edges with a significance of at least 50% of the maximum sig- nificance in the graph are drawn in orange
halfMaxSig <- max(E(graphNetwork)$sig) * 0.5
E(graphNetwork)$color <- ifelse(E(graphNetwork)$sig > halfMaxSig, "coral", "azure3")
# Disable edges with radius
E(graphNetwork)$curved <- 0 
# Size the nodes by their degree of networking
V(graphNetwork)$size <- log(degree(graphNetwork)) * 5
# All nodes must be assigned a standard minimum-size
V(graphNetwork)$size[V(graphNetwork)$size < 5] <- 3 
# edge thickness
E(graphNetwork)$width <- 2
# Define the frame and spacing for the plot
par(mai=c(0,0,1,0)) 
# Finaler Plot
plot(graphNetwork,				
     layout = layout.fruchterman.reingold,	# Force Directed Layout 
     main = paste(coocTerm, ' Graph'),
     vertex.label.family = "sans",
     vertex.label.cex = 0.8,
     vertex.shape = "circle",
     vertex.label.dist = 0.5,			# Labels of the nodes moved slightly
     vertex.frame.color = 'darkolivegreen',
     vertex.label.color = 'black',		# Color of node names
     vertex.label.font = 2,			# Font of node names
     vertex.label = V(graphNetwork)$name,		# node names
     vertex.label.cex = 1 # font size of node names 
)
# Set the graph and type. In this case, "F" means "Force Directed"
graphNetwork <- graph.data.frame(resultGraph, directed = F)
# Identification of all nodes with less than 2 edges
graphVs <- V(graphNetwork)[degree(graphNetwork) < 1]
# These edges are removed from the graph
graphNetwork <- delete.vertices(graphNetwork, graphVs) 
# Assign colors to edges and nodes (searchterm blue, rest orange)
V(graphNetwork)$color <- ifelse(V(graphNetwork)$name == coocTerm, 'cornflowerblue', 'orange') 
# Edges with a significance of at least 50% of the maximum sig- nificance in the graph are drawn in orange
halfMaxSig <- max(E(graphNetwork)$sig) * 0.5
E(graphNetwork)$color <- ifelse(E(graphNetwork)$sig > halfMaxSig, "coral", "azure3")
# Disable edges with radius
E(graphNetwork)$curved <- 0 
# Size the nodes by their degree of networking
V(graphNetwork)$size <- log(degree(graphNetwork)) * 5
# All nodes must be assigned a standard minimum-size
V(graphNetwork)$size[V(graphNetwork)$size < 5] <- 3 
# edge thickness
E(graphNetwork)$width <- 2
# Define the frame and spacing for the plot
par(mai=c(0,0,1,0)) 
# Finaler Plot
plot(graphNetwork,				
     layout = layout.fruchterman.reingold,	# Force Directed Layout 
     main = paste(coocTerm, ' Graph'),
     vertex.label.family = "sans",
     vertex.label.cex = 0.8,
     vertex.shape = "circle",
     vertex.label.dist = 0.5,			# Labels of the nodes moved slightly
     vertex.frame.color = 'darkolivegreen',
     vertex.label.color = 'black',		# Color of node names
     vertex.label.font = 2,			# Font of node names
     vertex.label = V(graphNetwork)$name,		# node names
     vertex.label.cex = 1 # font size of node names 
)
get_binDTM <- function(mycorpus) {
  require(Matrix)
  mycorpus <- reshape_corpus(mycorpus)
  mycorpus <- tm_map(mycorpus, removePunctuation, preserve_intra_word_dashes = TRUE)
  mycorpus <- tm_map(mycorpus, removeNumbers)
  mycorpus <- tm_map(mycorpus, content_transformer(tolower))
  mycorpus <- tm_map(mycorpus, removeWords, english_stopwords)
  mycorpus <- tm_map(mycorpus, stripWhitespace)
  
  binDTM1 <- DocumentTermMatrix(mycorpus, control=list(bounds = list(global=c(minimumFrequency, Inf)), weighting = weightBin))
  binDTM1 <- sparseMatrix(i = binDTM1$i, j = binDTM1$j, x = binDTM1$v, dims = c(binDTM1$nrow, binDTM1$ncol), dimnames = dimnames(binDTM1))
  return(binDTM1)
}
vis_cooc_network <- function(binDTM, coocTerm, graph_title) {
  require(igraph)
  coocs <- calculateCoocStatistics(coocTerm, binDTM, measure="LOGLIK")
  resultGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
  # The structure of the temporary graph object is equal to that of the resultGraph
  tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
  # Fill the data.frame to produce the correct number of lines
  tmpGraph[1:numberOfCoocs, 3] <- coocs[1:numberOfCoocs]
  # Entry of the search word into the first column in all lines
  tmpGraph[, 1] <- coocTerm
  # Entry of the co-occurrences into the second column of the respective line
  tmpGraph[, 2] <- names(coocs)[1:numberOfCoocs]
  # Set the significances
  tmpGraph[, 3] <- coocs[1:numberOfCoocs]
  # Attach the triples to resultGraph
  resultGraph <- rbind(resultGraph, tmpGraph)
  # Iteration over the most significant numberOfCoocs co-occurrences of the search term
  for (i in 1:numberOfCoocs) {
    # Calling up the co-occurrence calculation for term i from the search words co-occurrences
    newCoocTerm <- names(coocs)[i]
    coocs2 <- calculateCoocStatistics(newCoocTerm, binDTM, measure="LOGLIK")
    #print the co-occurrences
    coocs2[1:10]
    # Structure of the temporary graph object
    tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
    tmpGraph[1:numberOfCoocs, 3] <- coocs2[1:numberOfCoocs]
    tmpGraph[, 1] <- newCoocTerm
    tmpGraph[, 2] <- names(coocs2)[1:numberOfCoocs]
    tmpGraph[, 3] <- coocs2[1:numberOfCoocs]
    #Append the result to the result graph
    resultGraph <- rbind(resultGraph, tmpGraph[2:length(tmpGraph[, 1]), ])
  }
  # Set the graph and type. In this case, "F" means "Force Directed"
  graphNetwork <- graph.data.frame(resultGraph, directed = F)
  # Identification of all nodes with less than 2 edges
  graphVs <- V(graphNetwork)[degree(graphNetwork) < 2]
  # These edges are removed from the graph
  graphNetwork <- delete.vertices(graphNetwork, graphVs) 
  # Assign colors to edges and nodes (searchterm blue, rest orange)
  V(graphNetwork)$color <- ifelse(V(graphNetwork)$name == coocTerm, 'cornflowerblue', 'orange') 
  # Edges with a significance of at least 50% of the maximum sig- nificance in the graph are drawn in orange
  halfMaxSig <- max(E(graphNetwork)$sig) * 0.5
  E(graphNetwork)$color <- ifelse(E(graphNetwork)$sig > halfMaxSig, "coral", "azure3")
  # Disable edges with radius
  E(graphNetwork)$curved <- 0 
  # Size the nodes by their degree of networking
  V(graphNetwork)$size <- log(degree(graphNetwork)) * 5
  # All nodes must be assigned a standard minimum-size
  V(graphNetwork)$size[V(graphNetwork)$size < 5] <- 3 
  # edge thickness
  E(graphNetwork)$width <- 2
  # Define the frame and spacing for the plot
  par(mai=c(0,0,1,0)) 
  # Finaler Plot
  plot(graphNetwork,				
       layout = layout.fruchterman.reingold,	# Force Directed Layout 
       main = graph_title,
       vertex.label.family = "sans",
       vertex.label.cex = 0.8,
       vertex.shape = "circle",
       vertex.label.dist = 0.5,			# Labels of the nodes moved slightly
       vertex.frame.color = 'darkolivegreen',
       vertex.label.color = 'black',		# Color of node names
       vertex.label.font = 2,			# Font of node names
       vertex.label = V(graphNetwork)$name,		# node names
       vertex.label.cex = 1 # font size of node names 
  )
}
numberOfCoocs <- 15
minimumFrequency <- 10
c1 <- Corpus(VectorSource(textdata$text[as.integer(substr(textdata$date, 0, 4)) < 1950]))
c2 <- Corpus(VectorSource(textdata$text[as.integer(substr(textdata$date, 0, 4)) >= 1950]))
binDTM1 <- get_binDTM(c1)
binDTM2 <- get_binDTM(c2)
source("rscripts/calculateCoocStatistics.R")
vis_cooc_network(binDTM1, "california", "California (year < 1950)")
#vis_cooc_network(binDTM2, "california", "California (year >= 1950)")
