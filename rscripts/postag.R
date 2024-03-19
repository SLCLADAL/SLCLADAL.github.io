# function that pos-tags texts using udpipe

require(dplyr)
require(udpipe)

postag <- function(x, language){
  m <- udpipe::udpipe_load_model(udpipe::udpipe_download_model(language = language))

  # perform pos-tagging
  pos <- lapply(x, function(i){
    udpipe::udpipe_annotate(m, x = i) %>%
    as.data.frame() %>%
    dplyr::group_by(doc_id) %>%
    dplyr::summarise(postagged = paste(token, "/", xpos, collapse = " ", sep = "")) %>%
    dplyr::pull(postagged)

  })
  
  # add names
  names(pos) <- stringr::str_replace_all(names(pos), ".*/")
  names(pos) <- ifelse(is.null(names(pos)) == TRUE, 
                                paste0("text", 
                                       stringr::str_pad(1:length(pos), 
                                                        width = nchar(max(length(pos))), 
                                                        pad = "0")),
                                names(pos))
  
  # return tagged text
  return(pos)
}

# test
#sen <- c("This is an example sentence.")
#postag(x = sen, language = "english-ewt")


