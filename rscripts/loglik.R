# Function to calculate log-likelihood ratio for a 2x2 table
calculate_log_likelihood <- function(O11, O12, O21, O22) {
  N <- sum(O11, O12, O21, O22)
  
  # Calculate expected frequencies
  E11 <- (O11 + O12) * (O11 + O21) / N
  E12 <- (O11 + O12) * (O12 + O22) / N
  E21 <- (O21 + O22) * (O11 + O21) / N
  E22 <- (O21 + O22) * (O12 + O22) / N
  
  # Calculate log-likelihood ratio
  G2 <- 2 * (O11 * log(O11 / E11) + O12 * log(O12 / E12) + O21 * log(O21 / E21) + O22 * log(O22 / E22))
  
  return(G2)
}