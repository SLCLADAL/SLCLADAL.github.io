library(dplyr)
source("https://slcladal.github.io/rscripts/loglik.R")
assocstats <- function(x, term, coocfreq, termcoocfreq){
  x %>%
    # determine Term
  dplyr::filter(Term == term,
                # set minimum number of occurrences of CoocTerm
                CoocFreq > coocfreq,
                # set minimum number of co-occurrences of Term and CoocTerm
                TermCoocFreq > termcoocfreq) %>%
  # work row-wise
  dplyr::rowwise() %>%
  # calculate fishers' exact test
  dplyr::mutate(p = as.vector(unlist(fisher.test(matrix(c(a, b, c, d), 
                                                        ncol = 2, byrow = T))[1]))) %>%
  # extract x2 statistics
  dplyr::mutate(x2 = as.vector(unlist(chisq.test(matrix(c(a, b, c, d),   
                                                        ncol = 2, byrow = T),  simulate.p.value = TRUE)[1]))) %>%
  # extract expected frequency
  dplyr::mutate(expected = as.vector(unlist(chisq.test(matrix(c(a, b, c, d), 
                                                              ncol = 2, byrow = T),  simulate.p.value = TRUE)$expected[1]))) %>%
  # extract association measure phi
  dplyr::mutate(phi = sqrt((x2/(a + b + c + d)))) %>%
  # calculate mutual information (MI)
  dplyr::mutate(pTerm = TermFreq / AllFreq,
                pCoocTerm = CoocFreq / AllFreq,
                pTermAndCoocTerm = TermCoocFreq / AllFreq,
                MI = log2(pTermAndCoocTerm / (pTerm * pCoocTerm))) %>%
  # calculate odds ration
  dplyr::mutate(OddsRatio = ((a/b) / (c/d))) %>%
  # calculate loglikratio
  dplyr::mutate(LogLik = calculate_log_likelihood(a, b, c, d)) %>%
  # simplify significance
  dplyr::mutate(Significance = dplyr::case_when(p <= .001 ~ "p<.001",
                                                p <= .01 ~ "p<.01",
                                                p <= .05 ~ "p<.05", 
                                                TRUE ~ "n.s.")) %>%
  # round p-value
  dplyr::mutate(p = round(p, 5)) %>%
  # filter out non significant results
  dplyr::filter(Significance != "n.s.",
                # filter out instances where the Term and CoocTerm repel each other
                expected < a) %>%
  # arrange by phi (association measure)
  dplyr::arrange(-phi) %>%
  # remove superfluous columns
  dplyr::select(-pTerm, -pCoocTerm, -pTermAndCoocTerm, -TermCoocFreq,
              -AllFreq, -a, -b, -c, -d, -NRows) -> result
# inspect
return(result)
}
