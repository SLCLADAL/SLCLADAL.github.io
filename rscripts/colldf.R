# this function turns a text corpus into a tidy data frame with 
# frequencies of co-occurrences of terms with
# co-occurring words (based on co-occurrence in sentences)

colldf <- function(x){
  
  # step 1: clean and split corpus into sentences
  x %>% 
    # concatenate the elements in the 'text' object
    paste0(collapse = " ") %>%
    # separate possessives and contractions
    stringr::str_replace_all(fixed("'"), fixed(" '")) %>%
    stringr::str_replace_all(fixed("’"), fixed(" '")) %>%
    # split text into sentences
    tokenizers::tokenize_sentences() %>%
    # unlist sentences and save sentences in 'sentences' object
    unlist() -> sentences
  
  # convert sentences into a corpus object
  textcorpus <- Corpus(VectorSource(sentences))
  # convert to lower case
  extcorpusclean <- textcorpus %>%
  tm::tm_map(tolower)
  
  # step 2: convert corpus into a tidy data frame
  # create document term matrix
  textdtm <- DocumentTermMatrix(textcorpusclean, control=list(removePunctuation = TRUE,
                                                            removeNumbers = TRUE))
  # convert dtm into sparse matrix
  textsdtm <- Matrix::sparseMatrix(i = textdtm$i, j = textdtm$j, 
                                   x = textdtm$v, 
                                   dims = c(textdtm$nrow, textdtm$ncol),
                                   dimnames = dimnames(textdtm))
  # calculate co-occurrence counts
  coocurrences <- t(textsdtm) %*% textsdtm
  # convert into matrix
  collocates <- as.matrix(coocurrences)
  
  # prepare data for transformation into a tidy data frame with word 1, word 2, 
  # and their co-occurrence frequency
  words <- attr(collocates, "dimnames")[1] %>% 
    unlist()
  # extract and repeat word 1
  word1 <- rep(words, length(words))
  # extract and repeat word 2
  word2 <- rep(words, each = length(words))
  # extract co-occurrence frequency
  cooc_n <- as.numeric(c(collocates[!is.na(as.numeric(collocates))]))
  
  # create tidy data frame
  colls <- data.frame(word1, word2, cooc_n) %>%
  dplyr::rename(Term = 1,
                CoocTerm = 2,
                TermCoocFreq = 3)
  
  # step 3: reformat table nd extract frequency information for statistical analysis
  # extract stats
  collocation_df <- colls %>%
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
    dplyr::mutate(NRows = nrow(.))
  
  # step 4: return final data frame
  return(collocation_df)
}

