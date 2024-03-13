# required packages
require(here)
require(stringr)

# define function
loadtxts <- function(x){
  fls <- list.files(here::here("notebooks/MyTexts"), # path to the corpus data
                    
                    # full paths - not just the names of the files
                    full.names = T) 
  # loop over the vector 'myfiles' that contains paths to the data
  txts <- sapply(fls, function(x){
    
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
  
  # inspect the structure of the text object
  return(txts)
}
