# extract names vector of texts from data frame
# the data frame must have at least 
# a column entitled file and 
# a column entitled clean_text



# required packages
require(dplyr)


extracttext <- function(x){
  x %>%
    
    # group by file
    dplyr::group_by(file) %>%
    
    # concatenate the cleaned text into a single string for each group
    dplyr::summarise(text = paste0(text_clean, collapse = " ")) %>%
    
    # remove grouping
    dplyr::ungroup() %>%
    
    # extract the 'text' column
    dplyr::pull(text) %>%
    
    # remove extra spaces in the resulting character vector
    
    stringr::str_squish() -> ctext  # Assign the cleaned and formatted text to 'ctexts'
  
  # extract names
  x %>%
    
    # group by file
    dplyr::group_by(file) %>%
    
    # concatenate the cleaned text into a single string for each group
    dplyr::summarise(text = paste0(text_clean, collapse = " ")) %>%
    
    # remove grouping
    dplyr::ungroup() %>%
    
    # extract the 'text' column
    dplyr::pull(file) -> names(ctext)
  
  # add file extension
  names(ctext) <- paste0(names(ctext), ".txt")
  
  # return result
  return(ctext)

}
