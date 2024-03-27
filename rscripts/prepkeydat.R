# function to prepare data for a keyword analysis
# the function expects an obejct with two text objects
# the first text object is the target text
# the second text object is the reference object

prepkeydat <- function(text){
text1_words <- text[1] %>%
  # remove non-word characters
  stringr::str_remove_all("[^[:alpha:] ]") %>%
  # convert to lower
  tolower() %>%
  # tokenize the corpus files
  quanteda::tokens(remove_punct = T, 
                   remove_symbols = T,
                   remove_numbers = T) %>%
  # unlist the tokens to create a data frame
  unlist() %>%
  as.data.frame() %>%
  # rename the column to 'token'
  dplyr::rename(token = 1) %>%
  # group by 'token' and count the occurrences
  dplyr::group_by(token) %>%
  dplyr::summarise(n = n()) %>%
  # add column stating where the frequency list is 'from'
  dplyr::mutate(type = "text1")

text2_words <- text[2] %>%
  # remove non-word characters
  stringr::str_remove_all("[^[:alpha:] ]") %>%
  # convert to lower
  tolower() %>%
  # tokenize the corpus files
  quanteda::tokens(remove_punct = T, 
                   remove_symbols = T,
                   remove_numbers = T) %>%
  # unlist the tokens to create a data frame
  unlist() %>%
  as.data.frame() %>%
  # rename the column to 'token'
  dplyr::rename(token = 1) %>%
  # group by 'token' and count the occurrences
  dplyr::group_by(token) %>%
  dplyr::summarise(n = n()) %>%
  # add column stating where the frequency list is 'from'
  dplyr::mutate(type = "text2")

keytb <- dplyr::left_join(text1_words, text2_words, by = c("token")) %>%
  # rename columns and select relevant columns
  dplyr::rename(text1 = n.x,
                text2 = n.y) %>%
  dplyr::select(-type.x, -type.y) %>%
  # replace NA values with 0 in 'corpus' and 'kwic' columns
  tidyr::replace_na(list(text1 = 0, text2 = 0))
return(keytb)
}