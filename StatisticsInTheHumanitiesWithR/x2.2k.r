################################################################
### --- R skript "Chi-Squared Test for subtables of a 2 * k table"
### --- Author: Martin Schweinberger (01/2014)
### --- The script is based on:
### --- Bortz, Juergen, Gustav A. Lienert & Klaus Boehnke. 32008.
### --- Verteilungsfreie Methoden in der Biostatistik, 126-127.
### --- Heidelberg: Springer.
### --- R-Version: R version 3.0.1 (2013-05-16) -- "Good Sport"
### --- This skript performs a special kind of chi-squared test which is
### --- applied when testing if there is a significant difference
### --- between 2 rows in a table with exactly 2 columns
### --- and 3 or more rows.
################################################################
### --- Write a function to perform Chi-Square Tests for
### --- subtables in a 2*k table
### --- This function will only yield correct results for 2*k tables!
################################################################
### --- The function x2.2k performs a chi-squared test for
### --- subtables in a 2*k table and provides the common test statistics.
x2.2k <- function(x, row1, row2) {
  a1 <- x[row1, 1]
  b1 <- x[row1, 2]
  a2 <- x[row2, 1]
  b2 <- x[row2, 2]
  N1 <- sum(a1, b1)
  N2 <- sum(a2, b2)
  Na <- sum(x[, 1])
  Nb <- sum(x[, 2])
  N <- sum(as.vector(unlist(x)))
  head <- paste(rownames(x)[row1], " against ", rownames(x)[row2], " by ", colnames(x)[1], " vs ", colnames(x)[2], collapse = " ")
  chi <- round((N^2*((b2*a1)-(a2*b1))^2)/(Na*Nb*N2*N1*(N1+N2)), 3)
  df <- 1
  p <- round(pchisq(chi, 1, lower.tail = F), 4)
  phi <- round(sqrt(chi/N), 3)
  conclusion <- ifelse(p > 0.05, "Conclusion: the null hypothesis cannot be rejected! Results are not significant!",
    ifelse(p <= 0.05 & p > 0.01, "Preliminary conclusion: Test statistic is significant at alpha 0.05!",
    ifelse(p <= 0.01 & p > 0.001, "Preliminary conclusion: Test statistic is significant at alpha 0.01!",
    ifelse(p <= 0.001, "Preliminary conclusion: Test statistic is significant at alpha 0.001!", "Error: oh oh oh - somethings gone terribly wrong! sorry ..."
))))
  result <- list(head, chi, df, p, phi, conclusion)
  names(result) <- c("Description", "Chi-Squared", "df", "p-value", "Phi", "Report")

  return(result)  }

################################################################
### --- THE END
################################################################

################################################################
# Example
# Before performing the test, we will create a table
# on which we will apply the Chi Square Test.
# For now, we will create a test table, we call "test.tb"
#test.tb <- matrix(c(21, 14, 18, 13, 24, 12, 13, 30), ncol =2, byrow = T)
#colnames(test.tb) <- c("nicht erreicht", "erreicht")
#rownames(test.tb) <- c("röntgen weich", "röntgen hart", "beta-strahlen", "licht")
################################################################
# Perform test
#test <- x2.2k(test.tb, 1, 2)
# Call results
#test


