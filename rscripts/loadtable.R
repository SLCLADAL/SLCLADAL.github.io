# load and format xlsx for network analysis

# required packages
require(readxl)
require(here)
require(dplyr)

loadtable <- function(x){
  fls <- list.files(here::here(x), # path to the corpus data
                    # full paths - not just the names of the files
                    full.names = T) 
  
  # select own data (not test data)
  fls <- fls[!stringr::str_detect(fls, "test.xlsx")]
  
  # loop over the vector 'myfiles' that contains paths to the data
  dat <- readxl::read_xlsx(fls) 

  # return result
  return(dat)
  
}
