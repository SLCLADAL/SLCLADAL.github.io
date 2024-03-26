# function that prepares a table with the columns token, text1 and text2 for 
# extracting keynes statistics.

# load required packages
require(dplyr)

prepkey <- function(x){
  x %>%
  dplyr::mutate(text1 = as.numeric(text1),
                text2 = as.numeric(text2)) %>%
  dplyr::mutate(C1 = sum(text1),
                C2 = sum(text2),
                N = C1 + C2) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(R1 = text1+text2,
                R2 = N - R1,
                O11 = text1,
                O12 = R1-O11,
                O21 = C1-O11,
                O22 = C2-O12) %>%
  dplyr::mutate(E11 = (R1 * C1) / N,
                E12 = (R1 * C2) / N,
                E21 = (R2 * C1) / N,
                E22 = (R2 * C2) / N) %>%
  dplyr::select(-text1, -text2)  -> x
  return(x)
}