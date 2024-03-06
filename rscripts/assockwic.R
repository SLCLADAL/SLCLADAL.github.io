require(dplyr)
source("https://slcladal.github.io/rscripts/loglik.R")
assockwic <- function(x){
  x %>%
    # work row-wise
    dplyr::rowwise() %>%
    # add N
    dplyr::mutate(N = sum(O11, O12, O21, O22)) %>%
    # calculate fishers' exact test
    dplyr::mutate(p = as.vector(unlist(fisher.test(matrix(c(O11, O12, O21, O22), 
                                                          ncol = 2, byrow = T))[1]))) %>%
    
    # extract x2 statistics
    dplyr::mutate(x2 = as.vector(unlist(chisq.test(matrix(c(O11, O12, O21, O22),   
                                                          ncol = 2, byrow = T),  simulate.p.value = TRUE)[1]))) %>%
    # extract expected frequency
    
    dplyr::mutate(expected = as.vector(unlist(chisq.test(matrix(c(O11, O12, O21, O22), 
                                                                ncol = 2, byrow = T),  simulate.p.value = TRUE)$expected[1]))) %>%
    
    # extract association measures
    dplyr::mutate(phi = sqrt((x2 / N)),
                  dice = (2 * O11) / (2 * (O11 + O12 + O21)),
                  MI = (O11 / N) * 
                    log2((O11 / N) / ((O11+O12) / N) * 
                           ((O11+O21) / N) ),
                  PMI = log2( (O11 / N) / ((O11+O12) / N) * 
                                ((O11+O21) / N) ),
                  t = (O11 - expected) / sqrt(expected)) %>%
    
    # calculate odds ration
    dplyr::mutate(OddsRatio = ifelse(min(c(O11, O12, O21, O22) > 0),  ((O11/O12) / (O21/O22)), 0)) %>%
    
    # calculate loglikratio
    dplyr::mutate(LogLik = calculate_log_likelihood(O11, O12, O21, O22)) %>%
    
    # simplify significance
    dplyr::mutate(Significance = dplyr::case_when(p <= .001 ~ "p<.001",
                                                  p <= .01 ~ "p<.01",
                                                  p <= .05 ~ "p<.05", 
                                                  TRUE ~ "n.s.")) %>%
    # round p-value
    
    dplyr::mutate(p = round(p, 5)) %>%
    # filter out non significant results
    dplyr::filter(Significance != "n.s.",
                  # filter out instances where the w1 and w2 repel each other
                  expected < O11) %>%
    # arrange by phi (association measure)
    dplyr::arrange(-phi) %>%
    # remove superfluous columns
    dplyr::select(-any_of(c("TermCoocFreq", "AllFreq", "NRows"))) -> result
  # inspect
  return(result)
}
