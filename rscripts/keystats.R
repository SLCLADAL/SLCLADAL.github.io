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
    # calculate fishers' exact test
    dplyr::mutate(p = as.vector(unlist(fisher.test(matrix(c(O11, O12, O21, O22), 
                                                          ncol = 2, byrow = T))[1]))) %>%
    
    # extract AM
    # 1. bias towards top left
    dplyr::mutate(btl_O12 = ifelse(C1 > R1, 0, R1-C1),
                  btl_O11 = ifelse(C1 > R1, R1, R1-btl_O12),
                  btl_O21 = ifelse(C1 > R1, C1-R1, C1-btl_O11),
                  btl_O22 = ifelse(C1 > R1, C2, C2-btl_O12),
                  
                  # 2. bias towards top right
                  btr_O11 = 0, 
                  btr_O21 = R1,
                  btr_O12 = C1,
                  btr_O22 = C2-R1) %>%
    
    # 3. calculate AM
    dplyr::mutate(upp = btl_O11/R1,
                  low = btr_O11/R1,
                  op = O11/R1) %>%
    dplyr::mutate(AM = op / upp) %>%
    
    # remove superfluous columns
    dplyr::select(-any_of(c("btr_O21", "btr_O12", "btr_O22", "btl_O12", 
                            "btl_O11", "btl_O21", "btl_O22", "btr_O11"))) %>% 
    
    # extract descriptives
    dplyr::mutate(ptw_target = O11/C1*1000,
                  ptw_ref = O12/C2*1000) %>%
    
    # extract x2 statistics
    dplyr::mutate(X2 = (O11-E11)^2/E11 + (O12-E12)^2/E12 + (O21-E21)^2/E21 + (O22-E22)^2/E22) %>%
    
    # extract association measures
    dplyr::mutate(phi = sqrt((X2 / N)),
                  MI = log2(O11 / E11),
                  t.score = (O11 - E11) / sqrt(O11),
                  z.score = (O11 - E11) / sqrt(E11),
                  PMI = log2( (O11 / N) / ((O11+O12) / N) * 
                                ((O11+O21) / N) ),
                  DeltaP = (O11 / R1) - (O21 / R2),
                  LogOddsRatio = log(((O11 + 0.5) * (O22 + 0.5))  / ( (O12 + 0.5) * (O21 + 0.5) )),
                  G2 = 2 * (O11 * log(O11 / E11) + O12 * log(O12 / E12) + O21 * log(O21 / E21) + O22 * log(O22 / E22)),
                  
                  # traditional keyness measures
                  RateRatio = (O11/(C1*1000)) / (O12/(C2*1000)),
                  RateDifference = (O11/(C1*1000)) - (O12/(C2*1000)),
                  DifferenceCoefficient = RateDifference / sum((O11/(C1*1000)), (O12/(C2*1000))),
                  OddsRatio = ((O11 + 0.5) * (O22 + 0.5))  / ( (O12 + 0.5) * (O21 + 0.5) )) %>%
    
    # determine Bonferroni corrected significance
    dplyr::mutate(Sig_corrected = dplyr::case_when(p / Rws > .05 ~ "n.s.",
                                                   p / Rws > .01 ~ "p < .05*",
                                                   p / Rws > .001 ~ "p < .01**",
                                                   p / Rws <= .001 ~ "p < .001***",
                                                   T ~ "N.A.")) %>% 
    # round p-value
    dplyr::mutate(p = round(p, 5),
                  type = ifelse(E11 > O11, "antitype", "type")) %>%
    # filter out non significant results
    dplyr::filter(Sig_corrected != "n.s.") %>%
    # arrange by DeltaP (association measure)
    dplyr::arrange(-DeltaP) %>%
    # remove superfluous columns
    dplyr::select(-any_of(c("TermCoocFreq", "AllFreq", "NRows", 
                            "R1", "R2", "C1", "C2", "E12", "E21",
                            "E22", "upp", "low", "op", "t.score", "z.score", "Rws"))) %>%
    dplyr::relocate(any_of(c("token", "type", "Sig_corrected", "O11", "E11",
                             "ptw_target", "ptw_ref", "DeltaP", "LogOddsRatio", 
                             "AM",  "MI", "PMI", "phi", 
                             "X2", "G2", "RateRatio", "RateDifference",
                             "DifferenceCoefficient", "OddsRatio", 
                             "p", "O12", "O21", "O22"))) -> result
  # inspect
  return(result)
}
