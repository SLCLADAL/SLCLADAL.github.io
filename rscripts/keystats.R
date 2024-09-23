require(dplyr)
# x must have columns with 
# token: word 
# text1: frequency of token in text1
# text2: frequency of token in text2
keystats <- function(x){
  x %>%
    dplyr::mutate(text1 = as.numeric(text1),
                  text2 = as.numeric(text2)) %>%
    dplyr::mutate(C1 = sum(text1),
                  C2 = sum(text2),
                  N = C1 + C2) %>%
    # determine number of rows
    dplyr::mutate(Rws = nrow(.)) %>%   
    # work row-wise
    dplyr::rowwise() %>%
    dplyr::mutate(R1 = text1+text2,
                  R2 = N - R1,
                  O11 = text1,
                  O11 = ifelse(O11 == 0, O11+0.1, O11),
                  O12 = R1-O11,
                  O21 = C1-O11,
                  O22 = C2-O12) %>%
    dplyr::mutate(E11 = (R1 * C1) / N,
                  E12 = (R1 * C2) / N,
                  E21 = (R2 * C1) / N,
                  E22 = (R2 * C2) / N) %>%
    dplyr::select(-text1, -text2) %>%
    # calculate fishers' exact test
    dplyr::mutate(p = as.vector(unlist(fisher.test(matrix(c(O11, O12, O21, O22), 
                                                          ncol = 2, byrow = T))[1]))) %>%

    # extract descriptives
    dplyr::mutate(ptw_target = O11/C1*1000,
                  ptw_ref = O12/C2*1000) %>%
    
    # extract x2 statistics
    dplyr::mutate(X2 = (O11-E11)^2/E11 + (O12-E12)^2/E12 + (O21-E21)^2/E21 + (O22-E22)^2/E22) %>%
    
    # extract association measures
    dplyr::mutate(phi = sqrt((X2 / N)),
                  MI = log2(O11 / E11),
                  t.score = (O11 - E11) / sqrt(O11),
                  PMI = log2( (O11 / N) / ( C1 / N * R1 / N )),
                  DeltaP = (O11 / R1) - (O21 / R2),
                  LogOddsRatio = log(((O11 + 0.5) * (O22 + 0.5))  / ( (O12 + 0.5) * (O21 + 0.5) )),
                  G2 = 2 * ((O11+ 0.001) * log((O11+ 0.001) / E11) + (O12+ 0.001) * log((O12+ 0.001) / E12) + O21 * log(O21 / E21) + O22 * log(O22 / E22)),
                  
                  # traditional keyness measures
                  RateRatio = ((O11+ 0.001)/(C1*1000)) / ((O12+ 0.001)/(C2*1000)),
                  RateDifference = (O11/(C1*1000)) - (O12/(C2*1000)),
                  DifferenceCoefficient = RateDifference / sum((O11/(C1*1000)), (O12/(C2*1000))),
                  OddsRatio = ((O11 + 0.5) * (O22 + 0.5))  / ( (O12 + 0.5) * (O21 + 0.5) ),
                  LLR = 2 * (O11 * (log((O11 / E11)))),
                  RDF = abs((O11 / C1) - (O12 / C2)),
                  PDiff = abs(ptw_target - ptw_ref) / ((ptw_target + ptw_ref) / 2) * 100,
                  SignedDKL = sum(ifelse(O11 > 0, O11 * log(O11 / ((O11 + O12) / 2)), 0) - ifelse(O12 > 0, O12 * log(O12 / ((O11 + O12) / 2)), 0))) %>%
    
    # determine Bonferroni corrected significance
    dplyr::mutate(Sig_corrected = dplyr::case_when(p / Rws > .05 ~ "n.s.",
                                                   p / Rws > .01 ~ "p < .05*",
                                                   p / Rws > .001 ~ "p < .01**",
                                                   p / Rws <= .001 ~ "p < .001***",
                                                   T ~ "N.A.")) %>% 
    # round p-value
    dplyr::mutate(p = round(p, 5),
                  type = ifelse(E11 > O11, "antitype", "type"),
                  phi = ifelse(E11 > O11, -phi, phi),
                  G2 = ifelse(E11 > O11, -G2, G2)) %>%
    # filter out non significant results
    dplyr::filter(Sig_corrected != "n.s.") %>%
    # arrange by DeltaP (association measure)
    dplyr::arrange(-G2) %>%
    # remove superfluous columns
    dplyr::select(-any_of(c("TermCoocFreq", "AllFreq", "NRows", 
                            "R1", "R2", "C1", "C2", "E12", "E21",
                            "E22", "upp", "low", "op", "t.score", "z.score", "Rws"))) %>%
    dplyr::relocate(any_of(c("token", "type", "Sig_corrected", "O11", "O12",
                             "ptw_target", "ptw_ref", "G2",  "RDF", "RateRatio", 
                             "RateDifference", "DifferenceCoefficient", "LLR", "SignedDKL",
                             "PDiff", "LogOddsRatio", "MI", "PMI", "phi", "X2",  
                             "OddsRatio", "DeltaP", "p", "E11", "O21", "O22"))) -> result
  # inspect
  return(result)
}
