# function that prepares a table with the columns w1, w2 and O11 for 
# extracting association measures.

# load required packages
require(dplyr)

prepam <- function(x){
  x %>%  dplyr::mutate(N = sum(O11)) %>%
    
    # calculate R1, O12, and R2
    dplyr::group_by(w1) %>%
    dplyr::mutate(R1 = sum(O11),
                  O12 = R1 - O11,
                  R2 = N - R1) %>%
    dplyr::ungroup(w1) %>%
    
    # calculate C1, O21, C2, and O22
    dplyr::group_by(w2) %>%
    dplyr::mutate(C1 = sum(O11),
                  O21 = C1 - O11,
                  C2 = N - C1,
                  O22 = R2 - O21) -> x
  retunr(x)
}