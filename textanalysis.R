knitr::include_graphics("https://slcladal.github.io/images/uq1.jpg")

knitr::include_graphics("https://slcladal.github.io/images/gy_chili.jpg")

knitr::include_graphics("https://slcladal.github.io/images/GoogleNgram.png")

knitr::include_graphics("https://slcladal.github.io/images/romeonet.png")

## # install packages
## install.packages("DT")
## install.packages("knitr")
## install.packages("kableExtra")
## install.packages("quanteda")
## install.packages("tidyverse")
## install.packages("tm")
## install.packages("tidytext")
## install.packages("wordcloud2")
## install.packages("scales")
## install.packages("quanteda.textstats")
## install.packages("quanteda.textplots")
## install.packages("tidyr")
## install.packages("cluster")
## install.packages("class")
## install.packages("NLP")
## install.packages("openNLP")
## install.packages("openNLPdata")
## install.packages("pacman")
## install.packages("flextable")
## install.packages("http://datacube.wu.ac.at/src/contrib/openNLPmodels.en_1.5-1.tar.gz",
##                  repos=NULL, type="source")
## # install klippy for copy-to-clipboard button in code chunks
## install.packages("remotes")
## remotes::install_github("rlesur/klippy")

# set options
options(stringsAsFactors = F)
options(scipen = 999)
options(max.print=1000)
# load packages
library(tidyverse)
library(flextable)
library(quanteda)
library(tm)
library(tidytext)
library(wordcloud2)
library(scales)
library(quanteda.textstats)
library(quanteda.textplots)
library(tidyr)
library(cluster)
library(class)
library(NLP)
library(openNLP)
library(openNLPdata)
library(pacman)
pacman::p_load_gh("trinker/entity")
# activate klippy for copy-to-clipboard button
klippy::klippy()

knitr::include_graphics("https://slcladal.github.io/images/AntConcConcordance.png")

# load text
darwin <- base::readRDS(url("https://slcladal.github.io/data/darwin.rda", "rb"))

# inspect data
darwin %>%
  as.data.frame() %>%
  head(10) %>%
  flextable() %>%
  flextable::set_table_properties(width = .75, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "First 10 text elements of Charles Darwin's Origin")  %>%
  flextable::border_outer()

# combine and split into chapters
darwin_chapters <- darwin %>%
  # paste all texts together into one long text
  paste0(collapse = " ") %>%
  # replace Chapter I to Chapter XVI with qwertz 
  stringr::str_replace_all("(CHAPTER [XVI]{1,7}\\.{0,1}) ", "qwertz\\1") %>%
  # convert text to lower case
  tolower() %>%  
  # split the long text into chapters
  stringr::str_split("qwertz") %>%
  # unlist the result (convert into simple vector)
  unlist()

# inspect data
darwin_chapters %>%
  substr(start=1, stop=500) %>%
  as.data.frame() %>%
  head(5) %>%
  flextable() %>%
  flextable::set_table_properties(width = .95, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "First 500 characters of the first 5 chapters of Charles Darwin's Origin")  %>%
  flextable::border_outer()

# create kwic
kwic_o <- quanteda::kwic(x = darwin_chapters, # define text(s) 
                         # define pattern
                          pattern = "organism",
                         # define window size
                          window = 5) %>%
  # convert into a data frame
  as.data.frame() %>%
  # remove superfluous columns
  dplyr::select(-to, -from, -pattern)

# inspect data
kwic_o %>%
  as.data.frame() %>%
  head(10) %>%
  flextable() %>%
  flextable::set_table_properties(width = .95, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "First 10 concordances of organism in Charles Darwin's Origin")  %>%
  flextable::border_outer()

# create kwic
kwic_os <- quanteda::kwic(x = darwin_chapters, 
                          pattern = "organi.*",
                          window = 5,
                          valuetype = "regex") %>%
  # convert into a data frame
  as.data.frame() %>%
  # remove superfluous columns
  dplyr::select(-to, -from, -pattern)

# inspect data
kwic_o %>%
  as.data.frame() %>%
  head(10) %>%
  flextable() %>%
  flextable::set_table_properties(width = .95, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "First 10 cleaned Concordances of organism in Charles Darwin's Origin")  %>%
  flextable::border_outer()

# create kwic
kwic_ns <- quanteda::kwic(x = darwin_chapters, 
                          pattern = quanteda::phrase("natural selection"),
                          window = 5) %>%
  # convert into a data frame
  as.data.frame() %>%
  # remove superfluous columns
  dplyr::select(-to, -from, -pattern)

# inspect data
kwic_ns %>%
  as.data.frame() %>%
  head(10) %>%
  flextable() %>%
  flextable::set_table_properties(width = .95, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "First 10 cleaned Concordances of the phrase natural selection in Charles Darwin's Origin")  %>%
  flextable::border_outer()

# load and process corpus
darwin_words <- darwin  %>%
  # convert everything to lower case
  tolower() %>%
  # remove non-word characters
  str_replace_all("[^[:alpha:][:space:]]*", "")  %>%
  tm::removePunctuation() %>%
  stringr::str_squish() %>%
  stringr::str_split(" ") %>%
  unlist()

# inspect data
darwin_words %>%
  as.data.frame() %>%
  head(15) %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "First 15 words in Charles Darwin's Origin")  %>%
  flextable::border_outer()

# create table
wfreq <- darwin_words %>%
  table() %>%
  as.data.frame() %>%
  arrange(desc(Freq)) %>%
  dplyr::rename(word = 1,
                frequency = 2)

# inspect data
wfreq %>%
  as.data.frame() %>%
  head(15) %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "Top 15 words in Charles Darwin's Origin by frequency.")  %>%
  flextable::border_outer()

# create table wo stopwords
wfreq_wostop <- wfreq %>%
  anti_join(stop_words, by = "word") %>%
  dplyr::filter(word != "")

# inspect data
wfreq_wostop %>%
  as.data.frame() %>%
  head(15) %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "Top 15 lexical words in Charles Darwin's Origin by frequency.")  %>%
  flextable::border_outer()

wfreq_wostop %>%
  head(10) %>%
  ggplot(aes(x = reorder(word, -frequency, mean), y = frequency)) +
  geom_bar(stat = "identity") +
  labs(title = "10 most frequent non-stop words in \nCharles Darwin's Origin of Species",
       x = "") +
  theme(axis.text.x = element_text(angle = 45, size = 12, hjust = 1))

# create wordcloud
wordcloud2(wfreq_wostop[1:100,],
           shape = "diamond",
           color = scales::viridis_pal()(8)
          )

# load data
orwell_sep <- base::readRDS(url("https://slcladal.github.io/data/orwell.rda", "rb"))
orwell <- orwell_sep %>%
  paste0(collapse = " ")
melville_sep <- base::readRDS(url("https://slcladal.github.io/data/melville.rda", "rb"))
melville <- melville_sep %>%
  paste0(collapse = " ")
darwin_sep <- darwin
darwin <- paste0(darwin_sep, collapse = " ")

corp_dom <- quanteda::corpus(c(darwin, orwell, melville)) 
attr(corp_dom, "docvars")$Author = c("Darwin", "Orwell", "Melville")

corp_dom  %>%
    quanteda::tokens(remove_punct = TRUE) %>%
    quanteda::tokens_remove(stopwords("english")) %>%
    quanteda::dfm() %>%
    quanteda::dfm_group(groups = corp_dom$Author) %>%
    quanteda::dfm_trim(min_termfreq = 200, verbose = FALSE) %>%
    quanteda.textplots::textplot_wordcloud(comparison = TRUE, 
                                 max_words = 100,
                                 max_size = 6)

# extract number of words per chapter
Words <- darwin_chapters %>%
  stringr::str_split(" ")  %>%
  lengths()
# inspect data
Words

# extract number of matches per chapter
Matches <- darwin_chapters %>%
  stringr::str_count("organism[s]{0,1}")
# inspect the number of matches per chapter
Matches

# extract chapters
Chapters <- darwin_chapters %>%
  stringr::str_replace_all("(chapter [xvi]{1,7})\\.{0,1} .*", "\\1")
Chapters <- dplyr::case_when(nchar(Chapters) > 50 ~ "chapter 0", TRUE ~ Chapters)
Chapters

# create table of results
tb <- data.frame(Chapters, Matches, Words) %>%
  dplyr::mutate(Frequency = round(Matches/Words*1000, 2))

# inspect data
tb %>%
  as.data.frame() %>%
  head(15) %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "Words and their (relative) freqeuncy across in Charles Darwin's Origin by frequency.")  %>%
  flextable::border_outer()

# create plot
ggplot(tb, aes(x = Chapters, y = Frequency, group = 1)) + 
  geom_smooth(color = "purple") +
  geom_line(color = "darkgray") +         
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(name ="Relative Frequency (per 1,000 words)")

# add chapter names
names(darwin_chapters) <- Chapters
# generate corpus from chapters
darwin_corpus <- quanteda::corpus(darwin_chapters)
# generate dispersion plots
quanteda.textplots::textplot_xray(kwic(darwin_corpus, pattern = "organism"),
                                  kwic(darwin_corpus, pattern = "selection"),
                                  sort = T)

# generate and save dispersion plots
dp <- quanteda.textplots::textplot_xray(kwic(darwin_corpus, pattern = "organism"),
                                        kwic(darwin_corpus, pattern = "selection"))
# modify plot
dp + aes(color = keyword) + 
  scale_color_manual(values = c('red', 'blue')) +
  theme(legend.position = "none")

numbers <- matrix(c("you, your", "6761", "659", "Other words",	"259625",	
                    "105295", "Percent",	"2.60",	"0.63"), byrow = T, nrow = 3)
colnames(numbers) <- c("", "Private dialogues", "Scripted monologues")

# inspect data
ndf <- numbers %>%
  as.data.frame()
colnames(ndf)[1] <- "."
ndf %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "Use of 2nd person pronouns (and all other words) in ICE Ireland.")  %>%
  flextable::border_outer()

d <- matrix(c(6761, 659, 259625, 105295), nrow = 2, byrow = T)
colnames(d) <- c("D", "M")
rownames(d) <- c("you, your", "Other words")
assocplot(d)

# create data frame
darwin_bigrams <- data.frame(darwin_words[1:length(darwin_words)-1], 
                       darwin_words[2:length(darwin_words)]) %>%
  dplyr::rename(Word1 = 1,
                Word2 = 2) %>%
  dplyr::mutate(Bigram = paste0(Word1, " ", Word2)) %>%
  dplyr::group_by(Bigram) %>%
  dplyr::summarise(Frequency = n()) %>%
  dplyr::arrange(-Frequency)

# inspect data
darwin_bigrams %>%
  as.data.frame() %>%
  head(15) %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "Top 10 most frequent bigrams and their (relative) freqeuncy in Charles Darwin's Origin.")  %>%
  flextable::border_outer()

darwin_sentences <- darwin %>%
  tolower() %>%
  paste0(collapse= " ") %>%
  stringr::str_split(fixed(".")) %>%
  unlist() %>%
  tm::removePunctuation() %>%
  stringr::str_squish()

# inspect data
darwin_sentences %>%
  as.data.frame() %>%
  head(15) %>%
  flextable() %>%
  flextable::set_table_properties(width = .95, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "First 10 sentences in Charles Darwin's Origin.")  %>%
  flextable::border_outer()

# create a token object
darwin_tokens <- tokens(darwin_sentences, remove_punct = TRUE) %>%
  tokens_remove(stopwords("english"))
# extract collocations
darwin_coll <- textstat_collocations(darwin_tokens, size = 2, min_count = 20)

# inspect data
darwin_coll %>%
  as.data.frame() %>%
  head(15) %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "Top 10 collocations in Charles Darwin's Origin.")  %>%
  flextable::border_outer()

# create document-feature matrix
darwin_dfm <- darwin_sentences %>% 
    quanteda::dfm(remove = stopwords('english'), remove_punct = TRUE) %>%
    quanteda::dfm_trim(min_termfreq = 10, verbose = FALSE)

# inspect data
darwin_dfm[1:6, 1:6] %>%
  as.data.frame() %>%
  head(15) %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "First 6 rows and columns of the document-feature matrix.")  %>%
  flextable::border_outer()

# load function for co-occurrence calculation
source("https://slcladal.github.io/rscripts/calculateCoocStatistics.R")
# define term
coocTerm <- "organism"
# calculate co-occurrence statistics
coocs <- calculateCoocStatistics(coocTerm, darwin_dfm, measure="LOGLIK")
# inspect results
coocs[1:20]

redux_dfm <- dfm_select(darwin_dfm, 
                        pattern = c(names(coocs)[1:20], "organism"))

# inspect data
redux_dfm[1:6, 1:6] %>%
  as.data.frame() %>%
  head(15) %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "First 6 rows and columns of the reduced feature co-occurrence matrix.")  %>%
  flextable::border_outer()

tag_fcm <- fcm(redux_dfm)

# inspect data
tag_fcm[1:6, 1:6] %>%
  as.data.frame() %>%
  head(15) %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "First 6 rows and columns of the feature co-occurrence matrix.")  %>%
  flextable::border_outer()

# generate network graph
textplot_network(tag_fcm, 
                 min_freq = 1, 
                 edge_alpha = 0.1, 
                 edge_size = 5,
                 edge_color = "purple",
                 vertex_labelsize = log(rowSums(tag_fcm))*2)

corp_dom <- quanteda::corpus(c(darwin_sep, orwell_sep, melville_sep)) 
attr(corp_dom, "docvars")$Author = c(rep("Darwin", length(darwin_sep)), 
                                     rep("Orwell", length(orwell_sep)),
                                     rep("Melville", length(melville_sep)))

dfm_authors <- corp_dom %>%
  quanteda::tokens(remove_punct = TRUE) %>%
  quanteda::tokens_remove(quanteda::stopwords("english")) %>%
  quanteda::tokens_remove(c("now", "one", "like", "may", "can")) %>%
  quanteda::dfm() %>%
  quanteda::dfm_group(groups = Author) %>%
  quanteda::dfm_weight(scheme = "prop")

# Calculate relative frequency by president
freq_weight <- quanteda.textstats::textstat_frequency(dfm_authors, 
                                                      n = 10,
                                                      groups = dfm_authors$Author)

# inspect data
freq_weight %>%
  as.data.frame() %>%
  head(15) %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "Most common words across three texts.")  %>%
  flextable::border_outer()

ggplot(freq_weight, aes(nrow(freq_weight):1, frequency)) +
     geom_point() +
     facet_wrap(~ group, scales = "free") +
     coord_flip() +
     scale_x_continuous(breaks = nrow(freq_weight):1,
                        labels = freq_weight$feature) +
     labs(x = NULL, y = "Relative frequency")

# read in German text
German <- readLines("https://slcladal.github.io/data/phonemictext1.txt") %>%
  stringr::str_remove_all(" ") %>%
  stringr::str_split("") %>%
  unlist()
# inspect data
head(German, 20)

# read in texts
English <- readLines("https://slcladal.github.io/data/phonemictext2.txt")
Spanish <- readLines("https://slcladal.github.io/data/phonemictext3.txt")
Unknown <- readLines("https://slcladal.github.io/data/phonemictext4.txt")
# clean, split texts into phonemes, unlist and convert them into vectors
English <- as.vector(unlist(strsplit(gsub(" ", "", English), "")))
Spanish <- as.vector(unlist(strsplit(gsub(" ", "", Spanish), "")))
Unknown <- as.vector(unlist(strsplit(gsub(" ", "", Unknown), "")))
# inspect data
head(English, 20)

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
# simplify column names
colnames(German)[1:2] <- c("Phoneme", "Frequency")
colnames(English)[1:2] <- c("Phoneme", "Frequency")
colnames(Spanish)[1:2] <- c("Phoneme", "Frequency")
colnames(Unknown)[1:2] <- c("Phoneme", "Frequency")
# combine all tables into a single table
classdata <- rbind(German, English, Spanish, Unknown) 

# inspect data
classdata %>%
  as.data.frame() %>%
  head(10) %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "First 10 lines of the class data.")  %>%
  flextable::border_outer()

# convert into wide format
classdw <- classdata %>%
  spread(Phoneme, Frequency) %>%
  replace(is.na(.), 0)

# inspect data
classdw[, 1:6] %>%
  as.data.frame() %>%
  head(10) %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "Overview of the class data in wide format.")  %>%
  flextable::border_outer()

numvar <- colnames(classdw)[2:length(colnames(classdw))]
classdw[numvar] <- lapply(classdw[numvar], as.numeric)
# function for normalizing numeric variables
normalize <- function(x) { (x-min(x))/(max(x)-min(x))   }
# apply normalization
 classdw[numvar] <- as.data.frame(lapply(classdw[numvar], normalize))

# inspect data
classdw[, 1:6] %>%
  as.data.frame() %>%
  head(10) %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "Overview of the probabilities.")  %>%
  flextable::border_outer()

# remove language column
textm <- classdw[,2:ncol(classdw)]
# add languages as row names
rownames(textm) <- classdw[,1]
# create distance matrix
distmtx <- dist(textm)
# perform clustering
clustertexts <- hclust(distmtx, method="ward.D")  
# visualize cluster result
plot(clustertexts, hang = .25,main = "")           

# create training set
train <- classdw %>%
  filter(Language != "Unknown")
# create test set
test <- classdw %>%
  filter(Language == "Unknown")

# inspect data
classdw[, 1:6] %>%
  as.data.frame() %>%
  head(10) %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "Overview of the training set probabilities.")  %>%
  flextable::border_outer()

# set seed for reproducibility
set.seed(12345)
# apply k-nearest-neighbor (knn) classifier
prediction <- class::knn(train[,2:ncol(train)], 
                         test[,2:ncol(test)], 
                         cl = train[, 1], 
                         k = 3)
# inspect the result
prediction

# load corpus data
text <- readLines("https://slcladal.github.io/data/text4.txt", skipNul = T) %>%
  # clean data
  stringr::str_squish() %>%
  # remove empty elements
  .[. != ""]

# inspect data
text %>%
  as.data.frame() %>%
  head(1) %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "First paragraph of the text element (a description of Aldous Huxley's Brave New World)")  %>%
  flextable::border_outer()

# extract person entities
entity::person_entity(text)

## # extract locations
## entity::location_entity(text)
## entity::organization_entity(text)
## entity::date_entity(text)
## entity::money_entity(text)
## entity::percent_entity(text)

# convert text into string
text = as.String(text)
# define annotators
# sentence annotator
sent_annot = openNLP::Maxent_Sent_Token_Annotator()
# word annotator
word_annot = openNLP::Maxent_Word_Token_Annotator()

# location annotator
loc_annot = openNLP::Maxent_Entity_Annotator(kind = "location") 
# person annotator
people_annot = openNLP::Maxent_Entity_Annotator(kind = "person") 
# apply annotations
textanno = NLP::annotate(text, list(sent_annot, word_annot, 
                                        loc_annot, people_annot))

# extract features
k <- sapply(textanno$features, `[[`, "kind")
# extract locations
textlocations = names(table(text[textanno[k == "location"]]))
# extract people
textpeople = names(table(text[textanno[k == "person"]]))
# inspect extract people
textpeople

# function for pos-tagging
tagPOS <-  function(x, ...) {
  s <- as.String(x)
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- Annotation(1L, "sentence", 1L, nchar(s))
  a2 <- annotate(s, word_token_annotator, a2)
  a3 <- annotate(s, Maxent_POS_Tag_Annotator(), a2)
  a3w <- a3[a3$type == "word"]
  POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
  POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
  list(POStagged = POStagged)
  }

# load text
text <- readLines("https://slcladal.github.io/data/english.txt")
# pos tagging data
textpos <- tagPOS(text)
# inspect data
textpos

sessionInfo()
