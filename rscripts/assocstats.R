require(dplyr)
# x must have columns with 
# w1: word 1
# w2: word 2
# O11: co-occurrence frequency of w1 and w2
# O12: co-occurrence frequency of w1 without w2
# O21: frequency of w2 without w1
# O22: frequency of words in corpus without w1 and w2
assocstats <- function(x, term, coocfreq, termcoocfreq){
  x %>%
    # determine Term
  dplyr::filter(w1 == term,
                # set minimum number of occurrences of CoocTerm
                (O12+O22) > coocfreq,
                # set minimum number of co-occurrences of Term and CoocTerm
                O11 > termcoocfreq) %>%
    # work row-wise
    dplyr::rowwise() %>%
    # add N
    dplyr::mutate(N = sum(O11, O12, O21, O22),
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
    # round p-value

    dplyr::mutate(p = round(p, 5)) %>%
    # filter out non significant results
    dplyr::filter(Significance != "n.s.",
                # filter out instances where the w1 and w2 repel each other
                E11 < O11) %>%
    # arrange by phi (association measure)
    dplyr::arrange(-phi) %>%
    # remove superfluous columns
    dplyr::select(-any_of(c("TermCoocFreq", "AllFreq", "NRows"))) -> result
# inspect
return(result)
}
