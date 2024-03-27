# function that prepares a table with the columns w1, w2 and O11 for 
# extracting association measures.

# load required packages
require(dplyr)
require(stringr)
require(tokenizers)
require(quanteda)
require(tidytext)


prepam <- function(x){
  x %>% 
    # concatenate the elements in the 'text' object
    paste0(collapse = " ") %>%
    # separate possessives and contractions
    stringr::str_replace_all(fixed("'"), fixed(" '")) %>%
    stringr::str_replace_all(fixed("’"), fixed(" '")) %>%
    # split text into sentences
    tokenizers::tokenize_sentences() %>%
    # unlist sentences
    unlist() %>%
    # remove non-word characters
    stringr::str_replace_all("\\W", " ") %>%
    stringr::str_replace_all("[^[:alnum:] ]", " ") %>%
    # remove superfluous white spaces
    stringr::str_squish() %>%
    # convert to lower case and save in 'sentences' object
    tolower() %>%
  # tokenize the 'sentences' data using quanteda package
    quanteda::tokens() %>%
    # create a document-feature matrix (dfm) using quanteda
    quanteda::dfm() %>%
    # create a feature co-occurrence matrix (fcm) without considering trigrams
    quanteda::fcm(tri = FALSE) %>%
    # tidy the data using tidytext package
    tidytext::tidy() %>%
    # rearrange columns for better readability
    dplyr::relocate(term, document, count) %>%
    # rename columns for better interpretation
    dplyr::rename(w1 = 1,
                  w2 = 2,
                  O11 = 3) %>%  
    dplyr::mutate(N = sum(O11)) %>%
    
    # calculate R1, O12, and R2
    dplyr::group_by(w1) %>%
    dplyr::mutate(R1 = sum(O11),
                  O12 = R1 - O11,
                  R2 = N - R1) %>%
    dplyr::ungroup(w1) %>%
    
    # calculate C1, O21, C2, and O22
    dplyr::group_by(w2) %>%
    dplyr::mutate(C1 = sum(O11),
                  O21 = C1 - O11,
                  C2 = N - C1,
                  O22 = R2 - O21) -> x
  return(x)
}