# function that pos-tags texts using udpipe

require(dplyr)
require(udpipe)

postag <- function(x, language){
  m <- udpipe::udpipe_load_model(udpipe::udpipe_download_model(language = language))

  udpipe::udpipe_annotate(m, x = x) %>%
    as.data.frame() %>%
    dplyr::group_by(doc_id) %>%
    dplyr::summarise(postagged = paste(token, "/", xpos, collapse = " ", sep = "")) %>%
    dplyr::pull(postagged) -> pos
  # inspect tagged text
  return(pos)
}

# test
#sen <- c("This is an example sentence.")
#postag(x = sen, language = "english-ewt")


