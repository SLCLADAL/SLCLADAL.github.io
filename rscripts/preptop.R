require(tidytext)
require(stringr)
require(quanteda)
require(topicmodels)
require(seededlda)

preptop <- function(x) {
  x %>%
    # convert to lower case
    tolower() %>%
    # remove tags
    stringr::str_remove_all("<.*?>") %>%
    # remove superfluous white spaces
    stringr::str_squish() %>%
    # remove punctuation, numbers, and symbols
    quanteda::tokens(remove_punct = TRUE,
                     remove_numbers = TRUE, 
                     remove_symbol = TRUE) %>%
    # remove stopwords and selected other words
    quanteda::tokens_remove(pattern = c(stopwords("en"), "example", "words")) %>%
    # convert to a document frequency matrix
    quanteda::dfm() %>%
    quanteda::dfm_remove(min_nchar = 3) -> clean_dfm
  
  return(clean_dfm)
}