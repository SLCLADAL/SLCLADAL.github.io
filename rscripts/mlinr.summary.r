###########################################################################
### --- Function for Customized Multiple Linear Regression Results
###########################################################################
##################################################################
### --- R-Skript "Function for Customized Multiple Linear Regression Results"
### --- Author: Martin Schweinberger (June 18th, 2014)
### --- R-Version: R version 3.0.1 (2013-05-16) -- "Good Sport"
### --- This R script retrieves relevant information from regression outputs of
### --- Multiple Linear Regression.
### --- NOTE
### --- This script only works for Multiple Linear Regressions.
### --- The function takes three arguments: x = a lrm object, a = a glm object, 
### --- and dpvar = a vector representing the depentent variable with numeric values of either 0 or 1.
### --- CONTACT
### --- If you have questions,suggestions or you found errors
### --- or in case you would to provide feedback, questions
### --- write an email to
### --- martin.schweinberger.hh@gmail.com
### --- CITATION
### --- If you use this script or results thereof, please cite it as:
### --- Schweinberger, Martin. 2014. "Function for Customized
### --- Multiple Linear Regression Results ", unpublished R-skript,
### --- Hamburg University.
###############################################################
###                   START
###############################################################

mlr.summary <- function(mlr, glm, ia = T) {
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
    "VIF",
    "Std. beta", "CI(2.5%)", "CI(97.5%)",
    colnames(coefs)[2],
    colnames(coefs)[3],
    colnames(coefs)[4],
    "Significance")
  
  mdl.statz <- c("", "", "", "","", "", "", "", "Value")
  nbcases <- c("", "", "", "","", "", "", "", length(fitted(glm)))
  rste <- c("", "", "", "", "","", "", "", round(summary(mlr)[[6]], 2))
  mR <- c("", "", "", "", "","", "", "", round(summary(mlr)[[8]], 3))
  aR <- c("", "","", "", "", "", "", "", round(summary(mlr)[[9]], 3))    
  AIC <- c("", "", "", "", "","", "", "", round(summary(glm)[[5]], 2))
  BIC <- c("", "", "", "", "","", "", "", round(BIC(mlr), 2))
  f = summary(mlr)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)

Fstat <- c("", "", "", "","", paste("F-statistic:", round(summary(mlr)[[10]][[1]], 2)), 
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

