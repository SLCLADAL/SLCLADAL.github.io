require(dplyr)
require(tidyr)

combinefreq <- function(x, y){
  dplyr::left_join(x, y, by = c("token")) %>%
  # rename columns and select relevant columns
  dplyr::rename(corpus = n.x,
                kwic = n.y) %>%
  dplyr::select(-type.x, -type.y) %>%
  # replace NA values with 0 in 'corpus' and 'kwic' columns
  tidyr::replace_na(list(corpus = 0, kwic = 0)) %>%
  # calculate the difference between 'corpus' and 'kwic' and create a new column 'corpus'
  dplyr::mutate(corpuswokwic = corpus - kwic,
                corpuswokwic = ifelse(corpuswokwic < 0, 0, corpuswokwic))
}