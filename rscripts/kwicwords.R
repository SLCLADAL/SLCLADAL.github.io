require(dplyr)
require(tidyr)
require(quanteda)

kwicwords <- function(x){
  x %>%
  # Convert kwic_keyword to a data frame
  as.data.frame() %>%
  # select columns 'pre', 'keyword', and 'post'
  dplyr::select(pre, post) %>%
  # reshape the data frame from wide to long format using gather
  tidyr::gather(type, words, pre:post) %>%
  # extract the 'words' column and collapse into a single character string
  dplyr::pull(words) %>%
  paste0(collapse = " ") %>%
  # tokenize the text
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
  dplyr::mutate(type = "kwic")
}