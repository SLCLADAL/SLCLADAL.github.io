#' @title Summary Tables for Multiple Linear Regressions
#'
#' @description This function produces summary tables for fixed-effects multiple linear regressions by extracting the relevent information from a glm and an lm object.
#' @param x A glm object of family "binomial".
#' @param a A lrm object.
#' @param ia If ia = T (default) the table will show all columns of the summary table.
#' @export
#' @keywords multiple linear regression, linear regression, summary table, function
#' @seealso
#' @return NULL
#' @examples \dontrun{
#' Example code will come later!
#' }

mlinrsummary <- function(mlr, glm, ia = T) {
  options(warn=-1)
p.nice <- function(z) {
  as.vector(unlist(sapply(z, function(w) {
    ifelse(w < .001, return("p < .001***"),
    ifelse(w < .01, return("p < .01**"),
    ifelse(w < .05, return("p < .05*"), return(round(w, 4))))) } ))) }
cilwr <- confint(glm)[, 1]
ciupr <- confint(glm)[, 2]
  coefs <- summary(glm)[[12]]
  coef.df <- data.frame(
    round(coefs[, 1], 2),
    c("", round(vif(mlr), 2)),
 #   c("", round(lm.beta(mlr), 4)),
    round(cilwr, 2),
    round(ciupr, 2),
    round(coefs[, 2], 2),
    round(coefs[, 3], 2),
    round(coefs[, 4], 5),
    p.nice(coefs[, 4]))
  colnames(coef.df) <- c(colnames(coefs)[1],
    "VIF", "CI(2.5%)", "CI(97.5%)",
    colnames(coefs)[2],
    colnames(coefs)[3],
    colnames(coefs)[4],
    "Significance")
  
  mdl.statz <- c(rep("", 7), "Value")
  nbcases <- c(rep("", 7), length(fitted(glm)))
  rste <- c(rep("", 7), round(summary(mlr)[[6]], 2))
  mR <- c(rep("", 7), round(summary(mlr)[[8]], 3))
  aR <- c(rep("", 7), round(summary(mlr)[[9]], 3))    
  AIC <- c(rep("", 7), round(summary(glm)[[5]], 2))
  BIC <- c(rep("", 7), round(BIC(mlr), 2))
  f = summary(mlr)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)

Fstat <- c(rep("", 4), paste("F-statistic:", round(summary(mlr)[[10]][[1]], 2)), 
    paste("DF:", round(summary(mlr)[[10]][[2]], 0), "and", round(summary(mlr)[[10]][[3]], 0)), paste("p-value:", round(p[[1]], 5)),
    p.nice(p[[1]]))
  gblstz.tb <- rbind(mdl.statz, nbcases, rste, mR, aR, AIC, BIC, Fstat)
  gblstz.df <- as.data.frame(gblstz.tb)
  colnames(gblstz.df) <- colnames(coef.df)
  mlrm.tb <- rbind(coef.df, gblstz.df)
  rownames(mlrm.tb) <- c(rownames(coefs),
    "Model statistics", "Number of cases in model",  
    paste("Residual Standard Error", paste("on", summary(mlr)[[7]][[2]],"DF")),
    "Multiple R2", "Adjusted R2", "AIC", "BIC", "F-statistic")
  mlrm.tb1 <- as.data.frame(mlrm.tb)
  mlrm.tb2 <- as.data.frame(mlrm.tb[,-3])
  ifelse(ia == T, return(mlrm.tb2), return(mlrm.tb1))
}

