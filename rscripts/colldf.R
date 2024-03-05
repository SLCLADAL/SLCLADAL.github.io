# this function turns a text corpus into a tidy data frame with 
# frequencies of co-occurrences of terms with
# co-occurring words (based on co-occurrence in sentences)

colldf <- function(x){
  
  colldf <- x %>%
    quanteda::tokens() %>%
    quanteda::dfm() %>%
    quanteda::fcm(tri = FALSE) %>%
    tidytext::tidy() %>%
    dplyr::relocate(term, document, count) %>%
    dplyr::rename(w1 = 1,
                  w2 = 2,
                  O11 = 3) %>%
    # calculate N
    dplyr::mutate(N = sum(O11)) %>%
    # calculate R1
    dplyr::group_by(w1) %>%
    dplyr::mutate(R1 = sum(O11),
                  O12 = R1-O11,
                  R2 = N-R1) %>%
    dplyr::ungroup(w1) %>%
    # calculate C1
    dplyr::group_by(w2) %>%
    # calculate C1, O21, C2, O22
    dplyr::mutate(C1 = sum(O11),
                  O21 = C1-O11,
                  C2 = N-C1,
                  O22 = R2-O21) %>%
    # remove superfluous columns
    dplyr::select(-N, -R1, -R2, -C1, -C2)
  # return
  return(colldf)
  
  }

