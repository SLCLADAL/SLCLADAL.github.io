# load and format xlsx for network analysis

# required packages
require(dplyr)
require(ggplot2)
require(ggraph)
require(igraph)
require(here)
require(openxlsx)
require(readxl)
require(stringr)
require(tidyr)
require(tidytext)
require(tidygraph)

loadnetdata <- function(x){
  fls <- list.files(here::here(x), # path to the corpus data
                    # full paths - not just the names of the files
                    full.names = T) 
  
  # select own data (not test data)
  fls <- fls[!stringr::str_detect(fls, "test.xlsx")]
  
  # loop over the vector 'myfiles' that contains paths to the data
  ed <- readxl::read_xlsx(fls) 

  # return result
  return(ed)
  
  }
