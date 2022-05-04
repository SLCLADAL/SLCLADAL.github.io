knitr::include_graphics("https://slcladal.github.io/images/uq1.jpg")

knitr::include_graphics("https://slcladal.github.io/images/gy_chili.jpg")

## # set options
## options(stringsAsFactors = F)
## options(scipen = 999)
## options(max.print=1000)
## # install packages
## install.packages("cluster")
## install.packages("corpus")
## install.packages("FactoMineR")
## install.packages("factoextra")
## install.packages("flextable")
## install.packages("GGally")
## install.packages("ggdendro")
## install.packages("igraph")
## install.packages("network")
## install.packages("Matrix")
## install.packages("quanteda")
## install.packages("sna")
## install.packages("tidyverse")
## install.packages("tm")
## install.packages("tokenizers")
## install.packages("quanteda.textplots")
## install.packages("tidytext")
## install.packages(
##     "https://sfla.ch/wp-content/uploads/2021/02/collostructions_0.2.0.tar.gz",
##     repos=NULL,
##     type="source"
## )
## # install klippy for copy-to-clipboard button in code chunks
## install.packages("remotes")
## remotes::install_github("rlesur/klippy")

# load packages
library(cluster)
library(corpus)
library(FactoMineR)
library(factoextra)
library(flextable)
library(GGally)
library(ggdendro)
library(igraph)
library(network)
library(Matrix)
library(quanteda)
library(sna)
library(tidyverse)
library(tm)
library(tokenizers)
# activate klippy for copy-to-clipboard button
klippy::klippy()

# read in text
darwin <- base::readRDS(url("https://slcladal.github.io/data/cdo.rda", "rb")) %>%
  paste0(collapse = " ") %>%
  stringr::str_squish() %>%
  stringr::str_remove_all("- ")
# further processing
darwin_split <- darwin %>% 
  as_tibble() %>%
  tidytext::unnest_tokens(words, value)

darwin_split %>%
  as.data.frame() %>%
  head(15) %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "")  %>%
  flextable::border_outer()

# create data frame
darwin_words <- darwin_split %>%
  dplyr::rename(word1 = words) %>%
  dplyr::mutate(word2 = c(word1[2:length(word1)], NA)) %>%
  na.omit()

darwin_words %>%
  as.data.frame() %>%
  head(15) %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "")  %>%
  flextable::border_outer()

darwin2grams <- darwin_words %>%
  dplyr::mutate(bigram = paste(word1, word2, sep = " ")) %>%
  dplyr::group_by(bigram) %>%
  dplyr::summarise(frequency = n()) %>%
  dplyr::arrange(-frequency)

darwin2grams %>%
  as.data.frame() %>%
  head(15) %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "")  %>%
  flextable::border_outer()

# define stopwords
stps <- paste0(tm::stopwords(kind = "en"), collapse = "\\b|\\b")
# clean bigram table
darwin2grams_clean <- darwin2grams %>%
  dplyr::filter(!str_detect(bigram, stps))

darwin2grams_clean %>%
  as.data.frame() %>%
  head(15) %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "")  %>%
  flextable::border_outer()

#clean corpus
darwin_clean <- darwin %>%
  stringr::str_to_title()
# tokenize corpus
darwin_tokzd <- quanteda::tokens(darwin_clean)
# extract bigrams
BiGrams <- darwin_tokzd %>% 
       quanteda::tokens_remove(stopwords("en")) %>% 
       quanteda::tokens_select(pattern = "^[A-Z]", 
                               valuetype = "regex",
                               case_insensitive = FALSE, 
                               padding = TRUE) %>% 
       quanteda.textstats::textstat_collocations(min_count = 5, tolower = FALSE)

BiGrams %>%
  as.data.frame() %>%
  head(15) %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "")  %>%
  flextable::border_outer()

ngram_extract <- quanteda::tokens_compound(darwin_tokzd, pattern = BiGrams)

ngram_kwic <- kwic(ngram_extract, pattern = c("Natural_Selection", "South_America")) %>%
  as.data.frame() %>%
  dplyr::select(-to, -from, -pattern)

ngram_kwic %>%
  head(15) %>%
  flextable() %>%
  flextable::set_table_properties(width = .95, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "")  %>%
  flextable::border_outer()

# read in and process text
darwinsentences <- darwin %>%
  stringr::str_squish() %>%
  tokenizers::tokenize_sentences(.) %>%
  unlist() %>%
  stringr::str_remove_all("- ") %>%
  stringr::str_replace_all("\\W", " ") %>%
  stringr::str_squish()
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
  tm::tm_map(removePunctuation) %>%
  tm::tm_map(removeNumbers) %>%
  tm::tm_map(tolower) %>%
  tm::tm_map(removeWords, stopwords()) %>%
  tm::tm_map(removeWords, extrawords)
# create document term matrix
darwindtm <- DocumentTermMatrix(darwincorpusclean, control=list(bounds = list(global=c(1, Inf)), weighting = weightBin))

# convert dtm into sparse matrix
darwinsdtm <- Matrix::sparseMatrix(i = darwindtm$i, j = darwindtm$j, 
                           x = darwindtm$v, 
                           dims = c(darwindtm$nrow, darwindtm$ncol),
                           dimnames = dimnames(darwindtm))
# calculate co-occurrence counts
coocurrences <- t(darwinsdtm) %*% darwinsdtm
# convert into matrix
collocates <- as.matrix(coocurrences)

collocates[1:10, 1:10] %>%
  as.data.frame() %>%
   tibble::rownames_to_column("Word") %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "")  %>%
  flextable::bold(i = 1:10, j = 1, bold = TRUE, part = "body") %>%
  flextable::border_outer()

# inspect size of matrix
ncol(collocates)
summary(rowSums(collocates))

# load function for co-occurrence calculation
source("https://slcladal.github.io/rscripts/calculateCoocStatistics.R")
# define term
coocTerm <- "selection"
# calculate co-occurrence statistics
coocs <- calculateCoocStatistics(coocTerm, darwinsdtm, measure="LOGLIK")
# inspect results
coocs[1:20]

coocdf <- coocs %>%
  as.data.frame() %>%
  dplyr::mutate(CollStrength = coocs,
                Term = names(coocs)) %>%
  dplyr::filter(CollStrength > 30)

coocdf %>%
  as.data.frame() %>%
  dplyr::select(-.) %>%
  dplyr::relocate(Term, CollStrength) %>%
  head(15) %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "")  %>%
  flextable::border_outer()

ggplot(coocdf, aes(x = reorder(Term, CollStrength, mean), y = CollStrength)) +
  geom_point() +
  coord_flip() +
  theme_bw() +
  labs(y = "")

coolocs <- c(coocdf$Term, "selection")
# remove non-collocating terms
collocates_redux <- collocates[rownames(collocates) %in% coolocs, ]
collocates_redux <- collocates_redux[, colnames(collocates_redux) %in% coolocs]
# create distance matrix
distmtx <- dist(collocates_redux)

clustertexts <- hclust(    # hierarchical cluster object
  distmtx,                 # use distance matrix as data
  method="ward.D2")        # ward.D as linkage method

ggdendrogram(clustertexts) +
  ggtitle("Terms strongly collocating with *selection*")

net = network::network(collocates_redux, 
                       directed = FALSE,
                       ignore.eval = FALSE,
                       names.eval = "weights")
# vertex names
network.vertex.names(net) = rownames(collocates_redux)
# inspect object
net

ggnet2(net, 
       label = TRUE, 
       label.size = 4,
       alpha = 0.2,
       size.cut = 3,
       edge.alpha = 0.3) +
  guides(color = FALSE, size = FALSE)

# create vectors with collocation occurrences as categories
mid <- c("theory", "variations", "slight", "variation")
high <- c("natural", "selection")
infreq <- colnames(collocates_redux)[!colnames(collocates_redux) %in% mid & !colnames(collocates_redux) %in% high]
# add color by group
net %v% "Collocation" = ifelse(network.vertex.names(net) %in% infreq, "weak", 
                   ifelse(network.vertex.names(net) %in% mid, "medium", 
                   ifelse(network.vertex.names(net) %in% high, "strong", "other")))
# modify color
net %v% "color" = ifelse(net %v% "Collocation" == "weak", "gray60", 
                  ifelse(net %v% "Collocation" == "medium", "orange", 
                  ifelse(net %v% "Collocation" == "strong", "indianred4", "gray60")))
# rescale edge size
network::set.edge.attribute(net, "weights", ifelse(net %e% "weights" < 1, 0.1, 
                                   ifelse(net %e% "weights" <= 2, .5, 1)))
# define line type
network::set.edge.attribute(net, "lty", ifelse(net %e% "weights" <=.1, 3, 
                               ifelse(net %e% "weights" <= .5, 2, 1)))

ggnet2(net, 
       color = "color", 
       label = TRUE, 
       label.size = 4,
       alpha = 0.2,
       size = "degree",
       edge.size = "weights",
       edge.lty = "lty",
       edge.alpha = 0.2) +
  guides(color = FALSE, size = FALSE)

# perform correspondence analysis
res.ca <- CA(collocates_redux, graph = FALSE)
# plot results
fviz_ca_row(res.ca, repel = TRUE, col.row = "gray20")

# convert to data frame
coocdf <- as.data.frame(as.matrix(collocates))
# reduce data
diag(coocdf) <- 0
coocdf <- coocdf[which(rowSums(coocdf) > 10),]
coocdf <- coocdf[, which(colSums(coocdf) > 10)]
# extract stats
cooctb <- coocdf %>%
  dplyr::mutate(Term = rownames(coocdf)) %>%
  tidyr::gather(CoocTerm, TermCoocFreq,
                colnames(coocdf)[1]:colnames(coocdf)[ncol(coocdf)]) %>%
  dplyr::mutate(Term = factor(Term),
                CoocTerm = factor(CoocTerm)) %>%
  dplyr::mutate(AllFreq = sum(TermCoocFreq)) %>%
  dplyr::group_by(Term) %>%
  dplyr::mutate(TermFreq = sum(TermCoocFreq)) %>%
  dplyr::ungroup(Term) %>%
  dplyr::group_by(CoocTerm) %>%
  dplyr::mutate(CoocFreq = sum(TermCoocFreq)) %>%
  dplyr::arrange(Term) %>%
  dplyr::mutate(a = TermCoocFreq,
                b = TermFreq - a,
                c = CoocFreq - a, 
                d = AllFreq - (a + b + c)) %>%
  dplyr::mutate(NRows = nrow(coocdf))

cooctb %>%
  as.data.frame() %>%
  head(15) %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "")  %>%
  flextable::border_outer()

cooctb_redux <- cooctb %>%
  dplyr::filter(Term == coocTerm)

coocStatz <- cooctb_redux %>%
  dplyr::rowwise() %>%
  dplyr::mutate(p = as.vector(unlist(fisher.test(matrix(c(a, b, c, d), 
                                                        ncol = 2, byrow = T))[1]))) %>%
    dplyr::mutate(x2 = as.vector(unlist(chisq.test(matrix(c(a, b, c, d),                                                           ncol = 2, byrow = T))[1]))) %>%
  dplyr::mutate(phi = sqrt((x2/(a + b + c + d)))) %>%
      dplyr::mutate(expected = as.vector(unlist(chisq.test(matrix(c(a, b, c, d), ncol = 2, byrow = T))$expected[1]))) %>%
  dplyr::mutate(Significance = dplyr::case_when(p <= .001 ~ "p<.001",
                                                p <= .01 ~ "p<.01",
                                                p <= .05 ~ "p<.05", 
                                                FALSE ~ "n.s."))

coocStatz %>%
  as.data.frame() %>%
  head(15) %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "")  %>%
  flextable::border_outer()

coocStatz <- coocStatz %>%
  dplyr::ungroup() %>%
  dplyr::arrange(p) %>%
  dplyr::mutate(j = 1:n()) %>%
  # perform benjamini-hochberg correction
  dplyr::mutate(corr05 = ((j/NRows)*0.05)) %>%
  dplyr::mutate(corr01 = ((j/NRows)*0.01)) %>%
  dplyr::mutate(corr001 = ((j/NRows)*0.001)) %>%
  # calculate corrected significance status
  dplyr::mutate(CorrSignificance = dplyr::case_when(p <= corr001 ~ "p<.001",
                                                    p <= corr01 ~ "p<.01",
                                                    p <= corr05 ~ "p<.05", 
                                                    FALSE ~ "n.s.")) %>%
  dplyr::mutate(p = round(p, 6)) %>%
  dplyr::mutate(x2 = round(x2, 1)) %>%
  dplyr::mutate(phi = round(phi, 2)) %>%
  dplyr::arrange(p) %>%
  dplyr::select(-a, -b, -c, -d, -j, -NRows, -corr05, -corr01, -corr001) %>%
  dplyr::mutate(Type = ifelse(expected > TermCoocFreq, "Antitype", "Type"))

coocStatz %>%
  as.data.frame() %>%
  head(15) %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "")  %>%
  flextable::border_outer()

# load functions
source("https://SLCLADAL.github.io/rscripts/collexcovar.R")
# load data
ampaus <- base::readRDS(url("https://slcladal.github.io/data/ozd.rda", "rb"))

ampaus %>%
  as.data.frame() %>%
  head(15) %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "")  %>%
  flextable::border_outer()

# rename data
ampaus <- ampaus %>%
  dplyr::rename(keys = Variant, colls = Adjective, time = Age)
# perform analysis
collexcovar_really <- collexcovar(data = ampaus, keyterm = "really")

collexcovar_really %>%
  as.data.frame() %>%
  head(15) %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "")  %>%
  flextable::border_outer()

# perform analysis
collexcovar_pretty <- collexcovar(data = ampaus, keyterm = "pretty")
collexcovar_so <- collexcovar(data = ampaus, keyterm = "so")
collexcovar_very <- collexcovar(data = ampaus, keyterm = "very")

ampaus <- ampaus %>%
  dplyr::mutate(keys = ifelse(keys == "other", "bin", keys))
collexcovar_other <- collexcovar(data = ampaus, keyterm = "bin")

# combine tables
collexcovar_ampaus <- rbind(collexcovar_really, collexcovar_very, 
                     collexcovar_so, collexcovar_pretty, collexcovar_other)
collexcovar_ampaus <- collexcovar_ampaus %>%
  dplyr::rename(Age = time,
                Adjective = colls) %>%
  dplyr::mutate(Variant = ifelse(Variant == "bin", "other", Variant)) %>%
  dplyr::arrange(Age)

collexcovar_ampaus %>%
  as.data.frame() %>%
  head(15) %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "")  %>%
  flextable::border_outer()

ampauscoll <- collexcovar_ampaus %>%
  dplyr::select(Age, Adjective, Variant, Type, phi) %>%
  dplyr::mutate(phi = ifelse(Type == "Antitype", -phi, phi)) %>%
  dplyr::select(-Type) %>%
  tidyr::spread(Adjective, phi) %>%
  tidyr::replace_na(list(bad = 0,
                         funny = 0,
                         hard = 0,
                         good = 0,
                         nice = 0,
                         other = 0)) %>%
  tidyr::gather(Adjective, phi, bad:other) %>%
  tidyr::spread(Variant, phi) %>%
  tidyr::replace_na(list(pretty = 0,
                    really = 0,
                    so = 0,
                    very = 0,
                    other = 0)) %>%
  tidyr::gather(Variant, phi, other:very)

ampauscoll %>%
  as.data.frame() %>%
  head(15) %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "")  %>%
  flextable::border_outer()

p1 <- ggplot(ampauscoll, aes(x = reorder(Age, desc(Age)),
                       y = phi, group = Variant)) +
  facet_wrap(vars(Adjective)) +
  geom_line() +
  theme_set(theme_bw(base_size = 12)) +
  coord_cartesian(ylim = c(-.2, .4))

ggplot(ampauscoll, aes(x = reorder(Age, desc(Age)),
                       y = phi, group = Variant, 
                      color = Variant, linetype = Variant)) +
  facet_wrap(vars(Adjective)) +
  geom_line() +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  scale_color_manual(values = 
                       c("gray70", "gray70", "gray20", "gray70", "gray20"),
                        name="Variant",
                        breaks = c("other", "pretty", "really", "so", "very"), 
                        labels = c("other", "pretty", "really", "so", "very")) +
  scale_linetype_manual(values = 
                          c("dotted", "dotdash", "longdash", "dashed", "solid"),
                        name="Variant",
                        breaks = c("other", "pretty", "really", "so", "very"), 
                        labels = c("other",  "pretty", "really", "so", "very")) +
  theme(legend.position="top", 
        axis.text.x = element_text(size=12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  theme_set(theme_bw(base_size = 12)) +
  coord_cartesian(ylim = c(-.2, .4)) +
  labs(x = "Age", y = "Collocation Strength") +
  guides(size = FALSE)+
  guides(alpha = FALSE)

## # install devtools package
## install.packages("devtools")
## # load devtools package
## library(devtools)
## # install collostructions package
## install_local(here::here("renv/library/R-4.1/x86_64-w64-mingw32/collostructions_0.2.0.zip"), repos=NULL, type="source")

# load collostructions package
library(collostructions)

# draw a sample of the data
goVerb <- goVerb[sample(nrow(goVerb), 100),]

goVerb %>%
  as.data.frame() %>%
  head(15) %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "")  %>%
  flextable::border_outer()

# define corpus size
crpsiz <- sum(goVerb$CORP.FREQ)
# perform simple collexeme analysis
scollex_results <- collex(goVerb, corpsize = crpsiz, am = "logl", 
                          reverse = FALSE, decimals = 5,
                          threshold = 1, cxn.freq = NULL, 
                          str.dir = FALSE)

scollex_results %>%
  as.data.frame() %>%
  head(15) %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "")  %>%
  flextable::border_outer()

# load data
vsmdata <- base::readRDS(url("https://slcladal.github.io/data/vsd.rda", "rb")) %>%
  dplyr::mutate(Amplifier = ifelse(Amplifier == 0, 0, 1))

vsmdata %>%
  as.data.frame() %>%
  head() %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "")  %>%
  flextable::border_outer()

covar_results <- collex.covar(vsmdata)

covar_results %>%
  as.data.frame() %>%
  head(10) %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "")  %>%
  flextable::border_outer()

collexdist_results <- collex.dist(vsmdata, raw = TRUE)

collexdist_results %>%
  as.data.frame() %>%
  head(10) %>%
  flextable() %>%
  flextable::set_table_properties(width = .5, layout = "autofit") %>%
  flextable::theme_zebra() %>%
  flextable::fontsize(size = 12) %>%
  flextable::fontsize(size = 12, part = "header") %>%
  flextable::align_text_col(align = "center") %>%
  flextable::set_caption(caption = "")  %>%
  flextable::border_outer()

sessionInfo()
