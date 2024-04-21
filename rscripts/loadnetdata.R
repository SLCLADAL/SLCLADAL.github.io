
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
  dat <- readxl::read_xlsx(fls) 

  n1 <- dat
  n2 <- dat %>%
    dplyr::rename(from = to,
                  to = from)
  dat <- rbind(n1, n2) 
  
  # Create a co-occurrence matrix
  com <- dat %>%
    dplyr::group_by(to, from) %>%
    dplyr::summarise(count = n()) %>%
    spread(from, count, fill = 0) %>%
    as.data.frame()
  
  # Replace missing values with 0
  com[is.na(com)] <- 0
  
  # Print the co-occurrence matrix
  rownames(com) <- com[,1]
  com <- com[, 2:ncol(com)]
  
  # create a document feature matrix
  fcm <- quanteda::as.fcm(as.matrix(com))
  
  # return result
  return(fcm)
  
  }
=======
# load and format xlsx for network analysis

# required packages
require(readxl)
require(here)
require(dplyr)    # for table processing
require(openxlsx) # for reading and saving xlsx files
require(quanteda) # for generating dfm and fcm
require(quanteda.textplots)  # for generating network graphs 
require(ggplot2)  # for visualization
require(stringr)  # for text processing
require(tidyr)    # for table processing
require(tidytext)

loadnetdata <- function(x){
  fls <- list.files(here::here(x), # path to the corpus data
                    # full paths - not just the names of the files
                    full.names = T) 
  
  # select own data (not test data)
  fls <- fls[!stringr::str_detect(fls, "test.xlsx")]
  
  # loop over the vector 'myfiles' that contains paths to the data
  dat <- readxl::read_xlsx(fls) 

  n1 <- dat
  n2 <- dat %>%
    dplyr::rename(from = to,
                  to = from)
  dat <- rbind(n1, n2) 
  
  # Create a co-occurrence matrix
  com <- dat %>%
    dplyr::group_by(to, from) %>%
    dplyr::summarise(count = n()) %>%
    spread(from, count, fill = 0) %>%
    as.data.frame()
  
  # Replace missing values with 0
  com[is.na(com)] <- 0
  
  # Print the co-occurrence matrix
  rownames(com) <- com[,1]
  com <- com[, 2:ncol(com)]
  
  # create a document feature matrix
  fcm <- quanteda::as.fcm(as.matrix(com))
  
  # return result
  return(fcm)
  
  }
>>>>>>> 87f3e09316c3beb8033eaa33ed5371b0a4f40ba9
