require(dplyr)
require(tidytext)
require(syuzhet)

prepsenti <- function(x){
  senti <- data.frame(names(x), x) %>%
    dplyr::rename(file = 1, 
                  text = 2) %>%
    dplyr::mutate(file = stringr::str_remove_all(file, ".*/"),
                  file = stringr::str_remove_all(file, "\\..*")) %>%
    dplyr::group_by(file) %>%
    tidytext::unnest_tokens(word, text) %>%
    dplyr::mutate(words = n()) %>%
    dplyr::left_join(syuzhet::get_nrc_sentiment()) %>%
    dplyr::mutate(file = factor(file),
                  sentiment = factor(sentiment))
  return(senti)
}


