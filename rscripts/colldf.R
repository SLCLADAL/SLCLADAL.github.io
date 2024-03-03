# this function turns a text corpus into a tidy data frame with 
# frequencies of co-occurrences of terms with
# co-occurring words (based on co-occurrence in sentences)

colldf <- function(x){
  
# convert sentences into a corpus object
  textcorpus <- Corpus(VectorSource(x))

  # step 1: convert corpus into a tidy data frame
  # create document term matrix
  textcorpusdtm <- tm::DocumentTermMatrix(textcorpus)
  # convert dtm into sparse matrix
  textdtm <- Matrix::sparseMatrix(i = textcorpusdtm$i, j = textcorpusdtm$j, 
                                   x = textcorpusdtm$v, 
                                   dims = c(textcorpusdtm$nrow, textcorpusdtm$ncol),
                                   dimnames = dimnames(textcorpusdtm))
  # calculate co-occurrence counts
  coocs <- t(textdtm) %*% textdtm
  # convert into matrix
  coocs_mx <- as.matrix(coocs)
  
  # prepare data for transformation into a tidy data frame with word 1, word 2, 
  # and their co-occurrence frequency
  words <- attr(coocs_mx, "dimnames")[1] %>% 
    unlist()
  # extract and repeat word 1
  Term <- rep(words, length(words))
  # extract and repeat word 2
  CoocTerm <- rep(words, each = length(words))
  # extract co-occurrence frequency
  TermCoocFreq <- as.numeric(c(coocs_mx[!is.na(as.numeric(coocs_mx))]))
  
  # create tidy data frame
  colls <- data.frame(Term, CoocTerm, TermCoocFreq)
  
  # step 2: reformat table nd extract frequency information for statistical analysis
  # extract stats
  collocation_df <- colls %>%
    #dplyr::mutate(Term = factor(Term),
    #              CoocTerm = factor(CoocTerm)) %>%
    dplyr::mutate(AllFreq = sum(TermCoocFreq)) %>%
    dplyr::group_by(Term) %>%
    dplyr::mutate(TermFreq = sum(TermCoocFreq)) %>%
    dplyr::ungroup(Term) %>%
    dplyr::group_by(CoocTerm) %>%
    dplyr::mutate(CoocFreq = sum(TermCoocFreq)) %>%
    #dplyr::arrange(Term) %>%
    dplyr::mutate(a = TermCoocFreq,
                  b = TermFreq - a,
                  c = CoocFreq - a,
                  d = AllFreq - (a + b + c)) %>%
    dplyr::mutate(NRows = nrow(.))
  
  # step 3: return final data frame
  return(collocation_df)
}

