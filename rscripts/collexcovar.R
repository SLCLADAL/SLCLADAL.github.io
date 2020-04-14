# This function calculates repulsion and attractions between terms.
# The function requires a keyterm (e.g. a specific Variant) and a 
# data frame (data) with three columns labeled 
# colls (e.g. Adjectives), 
# keys (e.g. Variants),
# time (e.g. Age)
# time has to be a categorical variable.
collexcovar <- function(data, keyterm){
  ccla_result <- data %>%
  dplyr::mutate(key = ifelse(keys == keyterm, 1, 0),
                other = ifelse(keys != keyterm, 1, 0))  %>%
  dplyr::select(time, colls, key, other) %>%
  dplyr::group_by(time) %>%
  dplyr::mutate(Freq_AllColls = n(),
                Freq_AllColls_key = sum(as.numeric(as.character(key))),
                Freq_AllColls_other = sum(as.numeric(as.character(other)))
                ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(time, colls) %>%
  dplyr::summarise(Freq_AllColls = unique(Freq_AllColls),
                   Freq_AllColls_key = unique(Freq_AllColls_key),
                   Freq_AllColls_other = unique(Freq_AllColls_other),
                   Freq_key = sum(as.numeric(as.character(key))),
                   Freq_other = sum(as.numeric(as.character(other)))) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(Freq_Colls = sum(Freq_key+Freq_other)) %>%
  dplyr::mutate(a = Freq_key,
                b = Freq_AllColls_key-Freq_key,
                c = Freq_other,
                d = Freq_AllColls_other-Freq_other) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(p = as.vector(unlist(fisher.test(matrix(c(a, b, c, d), ncol = 2, byrow = T), simulate.p.value=TRUE)[1]))) %>%
    dplyr::mutate(x2 = as.vector(unlist(chisq.test(matrix(c(a, b, c, d), ncol = 2, byrow = T), simulate.p.value=TRUE)[1]))) %>%
  dplyr::mutate(phi = sqrt((x2/(a + b + c + d)))) %>%
      dplyr::mutate(expected = as.vector(unlist(chisq.test(matrix(c(a, b, c, d), ncol = 2, byrow = T), simulate.p.value=TRUE)$expected[1]))) %>%
  dplyr::mutate(Significance = ifelse(p <= .001, "p<.001",
                ifelse(p <= .01, "p<.01",
                       ifelse(p <= .05, "p<.05", "n.s."))))
ccla_result <- ccla_result %>%
  dplyr::mutate(NRows = nrow(ccla_result)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(p) %>%
  dplyr::mutate(j = 1:n()) %>%
  # perform benjamini-holm correction
  dplyr::mutate(corr05 = ((j/NRows)*0.05)) %>%
  dplyr::mutate(corr01 = ((j/NRows)*0.01)) %>%
  dplyr::mutate(corr001 = ((j/NRows)*0.001)) %>%
  # calculate corrected significance status
  dplyr::mutate(CorrSignificance = ifelse(p <= corr001, "p<.001",
                ifelse(p <= corr01, "p<.01",
                       ifelse(p <= corr001, "p<.001", "n.s.")))) %>%
  dplyr::mutate(p = round(p, 6)) %>%
  dplyr::mutate(x2 = round(x2, 1)) %>%
  dplyr::mutate(phi = round(phi, 2)) %>%
  dplyr::arrange(p) %>%
  dplyr::select(-a, -b, -c, -d, - j, -NRows, -corr05, -corr01, -corr001,
                -Significance, -Freq_AllColls, -Freq_AllColls_other,
                -Freq_AllColls_key) %>%
  dplyr::mutate(Type = ifelse(expected > Freq_key, "Antitype", "Type"))  %>%
  dplyr::mutate(Variant = keyterm)
return(ccla_result)
}
