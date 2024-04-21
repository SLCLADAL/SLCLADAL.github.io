require(dplyr)

sumsenti <- function(x) {
  senti_sum <- x %>%
    dplyr::group_by(file, sentiment) %>%
    dplyr::summarise(sentiment = unique(sentiment),
                     sentiment_freq = n(),
                     words = unique(words)) %>%
    dplyr::filter(is.na(sentiment) == F) %>%
    dplyr::mutate(percentage = round(sentiment_freq/words*100, 1)) %>%
    dplyr::mutate(sentiment = factor(sentiment, 
                                     levels = c("negative", "anger", "disgust", "fear", "sadness", "anticipation", "surprise", "trust", "joy", "positive"))) 
  
  return(senti_sum)
}