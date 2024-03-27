# required packages
require(here)
require(stringr)
require(dplyr)
require(ggplot2)
require(writexl)

# define function
loadkeytxts <- function(x="notebooks/Target", y="notebooks/Reference"){
  t_fls <- list.files(here::here(x), # path to the corpus data
                    
                    # full paths - not just the names of the files
                    full.names = T) 
  #remove test.txt from file list
  t_fls <- t_fls[!stringr::str_detect(t_fls, ".*test.txt")]

  r_fls <- list.files(here::here(y), # path to the corpus data
                      
                      # full paths - not just the names of the files
                      full.names = T) 
  #remove test.txt from file list
  r_fls <- r_fls[!stringr::str_detect(r_fls, ".*test.txt")]
  
    
  # loop over the vector 'myfiles' that contains paths to the data
  text1 <- sapply(t_fls, function(x){
    
    # read the content of each file using 'scan'
    x <- scan(x, 
              what = "char",    # specify that the input is characters
              sep = "",         # set separator to an empty string (read entire content)
              quote = "",       # set quote to an empty string (no quoting)
              quiet = T,        # suppress scan messages
              skipNul = T)      # skip NUL bytes if encountered
    
    # combine the character vector into a single string with spaces
    x <- paste0(x, sep = " ", collapse = " ")
    
    # remove extra whitespaces using 'str_squish' from the 'stringr' package
    x <- stringr::str_squish(x)
  })
  text1 <- paste0(unlist(text1), sep = " ", collapse = " ")


  # loop over the vector 'myfiles' that contains paths to the data
  text2 <- sapply(r_fls, function(x){
    
    # read the content of each file using 'scan'
    x <- scan(x, 
              what = "char",    # specify that the input is characters
              sep = "",         # set separator to an empty string (read entire content)
              quote = "",       # set quote to an empty string (no quoting)
              quiet = T,        # suppress scan messages
              skipNul = T)      # skip NUL bytes if encountered
    
    # combine the character vector into a single string with spaces
    x <- paste0(x, sep = " ", collapse = " ")
    
    # remove extra whitespaces using 'str_squish' from the 'stringr' package
    x <- stringr::str_squish(x)
  }) 
  text2 <- paste0(unlist(text2), sep = " ", collapse = " ")
    
  # inspect the structure of the text object
  return(c(text1, text2))
}
