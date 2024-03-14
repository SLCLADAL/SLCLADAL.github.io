# reformat text to data frame

# load required packages
require(dplyr)
require(stringr)

# define function that re-formats text to a data frame
formattext <- function(x){
  # extract names of texts
  nms <- names(x) %>%
    stringr::str_remove_all(".*/") %>%
    stringr::str_remove_all(".txt")
  
  # create a data frame of split texts
  text_tbl <- unlist(x) %>% tibble()
  # add the file names
  text_df <- data.frame(nms, text_tbl) %>%
  # add column names
  dplyr::rename(file = 1, 
                text = 2)
  
  # return df
  return(text_df)
}
