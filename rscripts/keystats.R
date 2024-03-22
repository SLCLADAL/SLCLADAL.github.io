require(dplyr)
# x must have columns with 
# token: word 
# corpus1: frequency of token in corpus1
# corpus2: frequency of token in corpus2
assockwic <- function(x){
  x %>%
    dplyr::mutate(corpus2 = as.numeric(corpus2),
                  corpus1 = as.numeric(corpus1)) %>%
    dplyr::mutate(C1 = sum(corpus1),
                  C2 = sum(corpus2),
                  N = C1 + C2) %>%
    # determine number of rows
    dplyr::mutate(Rws = nrow(.)) %>% 
    dplyr::rowwise() %>%
    dplyr::mutate(R1 = corpus+corpus1,
                  R2 = N - R1,
                  O11 = corpus1,
                  O12 = R1-O11,
                  O21 = C1-O11,
                  O22 = C2-O12) %>%
    dplyr::mutate(E11 = (R1 * C1) / N,
                  E12 = (R1 * C2) / N,
                  E21 = (R2 * C1) / N,
                  E22 = (R2 * C2) / N) %>%
    dplyr::select(-corpus, -kwic) %>%
    # calculate fishers' exact test
    dplyr::mutate(p = as.vector(unlist(fisher.test(matrix(c(O11, O12, O21, O22), 
                                                          ncol = 2, byrow = T))[1]))) %>%
    
    
    # extract x2 statistics
    dplyr::mutate(X2 = (O11-E11)^2/E11 + (O12-E12)^2/E12 + (O21-E21)^2/E21 + (O22-E22)^2/E22) %>%
    
    # extract association measures
    dplyr::mutate(phi = sqrt((X2 / N)),
                  Dice = (2 * O11) / (R1 + C1),
                  MI = log2(O11 / E11),
                  MS = min((O11/C1), (O11/R1)),
                  t.score = (O11 - E11) / sqrt(O11),
                  z.score = (O11 - E11) / sqrt(E11),
                  PMI = log2( (O11 / N) / ((O11+O12) / N) * 
                                ((O11+O21) / N) ),
                  DeltaP12 = (O11 / (O11 + O12)) - (O21 / (O21 + O22)),
                  DeltaP21 =  (O11 / (O11 + O21)) - (O21 / (O12 + O22)),
                  DP = (O11 / R1) - (O21 / R2),
                  OddsRatio = log(((O11 + 0.5) * (O22 + 0.5))  / ( (O12 + 0.5) * (O21 + 0.5) )),
                  # calculate LL aka G2
                  G2 = 2 * (O11 * log(O11 / E11) + O12 * log(O12 / E12) + O21 * log(O21 / E21) + O22 * log(O22 / E22))) %>%
    
    
    # simplify significance
    dplyr::mutate(Significance = dplyr::case_when(p <= .001 ~ "p<.001",
                                                  p <= .01 ~ "p<.01",
                                                  p <= .05 ~ "p<.05", 
                                                  TRUE ~ "n.s.")) %>%
    # determine Bonferroni corrected significance
    dplyr::mutate(Sig_corrected = dplyr::case_when(p / Rws > .05 ~ "n.s.",
                                                   p / Rws > .01 ~ "p < .05*",
                                                   p / Rws > .001 ~ "p < .01**",
                                                   p / Rws <= .001 ~ "p < .001***",
                                                   T ~ "N.A.")) %>%  
    # round p-value
    dplyr::mutate(p = round(p, 5)) %>%
    # filter out non significant results
    dplyr::filter(Sig_corrected != "n.s.",
                  # filter out instances where the w1 and w2 repel each other
                  E11 < O11) %>%
    # arrange by phi (association measure)
    dplyr::arrange(-phi) %>%
    # remove superfluous columns
    dplyr::select(-any_of(c("TermCoocFreq", "AllFreq", "NRows", "O12", "O21", 
                            "O22", "R1", "R2", "C1", "C2"))) -> result
  # inspect
  return(result)
}
