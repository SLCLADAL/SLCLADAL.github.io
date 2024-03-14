# load and format xlsx for network analysis

# required packages
required(readxl)

loadnetdata <- function(x){
  fls <- list.files(here::here(x), # path to the corpus data
                    # full paths - not just the names of the files
                    full.names = T) 
  
  # loop over the vector 'myfiles' that contains paths to the data
  dat <- sapply(fls, function(y){
    
    # read the content of each file using 'read_xlsx' from the 'readxl' package
    y <- readxl::read_xlsx(y) 
    })

  # add row names
  rownames(dat) <- colnames(dat)
  
  # return result
  return(dat)
  
  }
