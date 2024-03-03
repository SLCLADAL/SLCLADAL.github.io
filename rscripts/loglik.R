# Function to calculate log-likelihood ratio for a 2x2 table
calculate_log_likelihood <- function(a, b, c, d) {
  N <- a + b + c + d
  
  # Calculate expected frequencies
  E11 <- (a + b) * (a + c) / N
  E12 <- (a + b) * (b + d) / N
  E21 <- (c + d) * (a + c) / N
  E22 <- (c + d) * (b + d) / N
  
  # Calculate log-likelihood ratio
  G2 <- 2 * (a * log(a / E11) + b * log(b / E12) + c * log(c / E21) + d * log(d / E22))
  
  return(G2)
}