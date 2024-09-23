require(dplyr)
# x must have columns with 
# w1: word 1
# w2: word 2
# O11: co-occurrence frequency of w1 and w2
# O12: co-occurrence frequency of w1 without w2
# O21: frequency of w2 without w1
# O22: frequency of words in corpus without w1 and w2
assocstats <- function(x, term){
  x %>%
    # determine number of rows
    dplyr::mutate(Rws = nrow(.)) %>% 
    # work row-wise
    dplyr::rowwise() %>%
    # add N
    dplyr::mutate(O11 = ifelse(O11 == 0, O11+0.1, O11),
                  N = sum(O11, O12, O21, O22),
                  R1 = O11 + O12,
                  R2 = O21 + O22,
                  C1 = O11 + O21,
                  C2 = O12 + O22) %>%
    dplyr::mutate(E11 = R1 * C1 / N, 
                  E12 = R1 * C2 / N,
                  E21 = R2 * C1 / N, 
                  E22 = R2 * C2 / N) %>%
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
                  btr_O12 = ifelse(C2 > R1, 0, R1-C2),
                  btr_O11 = ifelse(C2 > R1, R1, R1-btl_O12),
                  btr_O21 = ifelse(C2 > R1, C2-R1, C2-btl_O11),
                  btr_O22 = ifelse(C2 > R1, C2, C2-btl_O12)) %>%
    
    # 3. calculate AM
    dplyr::mutate(upp = btl_O11/R1,
                  low = btr_O11/R1,
                  op = O11/R1) %>%
    dplyr::mutate(AM = op / upp) %>%
    
    # remove superfluous columns
    dplyr::select(-any_of(c("btr_O21", "btr_O12", "btr_O22", "btl_O12", 
                            "btl_O11", "btl_O21", "btl_O22", "btr_O11"))) %>% 

    # extract x2 statistics
    dplyr::mutate(X2 = (O11-E11)^2/E11 + (O12-E12)^2/E12 + (O21-E21)^2/E21 + (O22-E22)^2/E22) %>%
    
    # extract association measures
    dplyr::mutate(phi = sqrt((X2 / N)),
                  Dice = (2 * O11) / (R1 + C1),
                  LogDice = log((2 * O11) / (R1 + C1)),
                  MI = log2(O11 / E11),
                  MS = min((O11/C1), (O11/R1)),
                  t.score = (O11 - E11) / sqrt(O11),
                  z.score = (O11 - E11) / sqrt(E11),
                  PMI = log2( (O11 / N) / ( C1 / N * R1 / N )),
                  DeltaP12 = (O11 / (O11 + O12)) - (O21 / (O21 + O22)),
                  DeltaP21 =  (O11 / (O11 + O21)) - (O21 / (O12 + O22)),
                  DP = (O11 / R1) - (O21 / R2),
                  LogOddsRatio = log(((O11 + 0.5) * (O22 + 0.5))  / ( (O12 + 0.5) * (O21 + 0.5) )),
                  # calculate LL aka G2
                  G2 = 2 * (O11 * log(O11 / E11) + O12 * log(O12 / E12) + O21 * log(O21 / E21) + O22 * log(O22 / E22))) %>%
    
    # determine Bonferroni corrected significance
    dplyr::mutate(Sig_corrected = dplyr::case_when(p / Rws >= .05 ~ "n.s.",
                                                   p / Rws >= .01 ~ "p < .05*",
                                                   p / Rws >= .001 ~ "p < .01**",
                                                   p / Rws <= .001 ~ "p < .001***",
                                                   T ~ "N.A.")) %>%
    # round p-value
    dplyr::mutate(p = round(p, 5)) %>%
    # filter out non significant results
    dplyr::filter(# filter out instances where the w1 and w2 repel each other
                E11 < O11) %>%
    # arrange by AM (association measure)
    dplyr::arrange(-DeltaP12) %>%
    # remove superfluous columns
    dplyr::select(-any_of(c("TermCoocFreq", "AllFreq", "NRows", "O12", "O21", 
                            "O22", "R1", "R2", "C1", "C2", "E11", "E12", "E21",
                            "E22", "upp", "low", "op", "Rws"))) -> result
# inspect
return(result)
}
