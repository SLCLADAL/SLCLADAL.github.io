require(dplyr)
require(quanteda)

# extract and count unique tokens in the corpus data
# convert kwic_keyword to a data frame showing each token and its frequency
corpuswords <- function(x){
  x %>%
  # tokenize the corpus files
  quanteda::tokens() %>%
  # unlist the tokens to create a data frame
  unlist() %>%
  as.data.frame() %>%
  # rename the column to 'token'
  dplyr::rename(token = 1) %>%
  # group by 'token' and count the occurrences
  dplyr::group_by(token) %>%
  dplyr::summarise(n = n()) %>%
  # add column stating where the frequency list is 'from'
  dplyr::mutate(type = "corpus")
}