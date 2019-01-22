###########################################################################
### --- Function for Customized Binomial Logistic Regression Results
###########################################################################
##################################################################
### --- R script "Function for Customized Binomial Logistic Regression Results"
### --- Author: Martin Schweinberger (June 18th, 2014)
### --- R-Version: R version 3.0.1 (2013-05-16) -- "Good Sport"
### --- This R script retrieves relevant information from regression outputs of
### --- Binomial Logistic Regression.
### --- NOTE
### --- This script only works for Binomial Logistic Regressions.
### --- The function takes two arguments: x = a glm object; a = a lrm object;
### --- a percentage value representing the accuracy of the prediction achieved
### --- by the model.
### --- CONTACT
### --- If you have questions,suggestions or you found errors
### --- or in case you would to provide feedback, questions
### --- write an email to
### --- martin.schweinberger.hh@gmail.com
### --- CITATION
### --- If you use this script or results thereof, please cite it as:
### --- Schweinberger, Martin. 2014. "Function for Customized
### --- Binomial Logistic Regression Results ", unpublished R-skript,
### --- Hamburg University.
###############################################################
###                   START
###############################################################

###############################################################
### Function for a neat output table for Multiple Linear Regression Models
###############################################################
# x = glm model, a = lrm model, accuracy = predict.acc
blrm.summary <- function(x, a, accuracy) {
p.nice <- function(z) {
  as.vector(unlist(sapply(z, function(w) {
    ifelse(w < .001, return("p < .001***"),
    ifelse(w < .01, return("p < .01**"),
    ifelse(w < .05, return("p < .05*"), return(round(w, 4))))) } ))) }
  coefs <- summary(x)[[12]]
  ci <- exp(confint(x))
  cilwr <- ci[, 1]
  ciupr <- ci[, 2]
  coef.df <- data.frame(
    round(coefs[, 1], 2),
    c("", round(vif(x), 2)),
    round(exp(coefs[, 1]), 2),
    round(cilwr, 2),
    round(ciupr, 2),
    round(coefs[, 2], 2),
    round(coefs[, 3], 2),
    round(coefs[, 4], 5),
    p.nice(coefs[, 4]))
  colnames(coef.df) <- c(colnames(coefs)[1],
    "VIF",
    "OddsRatio",
    "CI(2.5%)",
    "CI(97.5%)",
    colnames(coefs)[2],
    colnames(coefs)[3],
    colnames(coefs)[4],
    "Significance")
  mdl.statz <- c("", "", "", "", "", "", "", "", "Value")
  nbcases <- c("", "", "", "", "", "", "", "", length(summary(x)[[11]]))
  obs0 <- c("", "", "", "", "", "", "", paste(names(a[[1]][1]), ":", sep = " ", collapse = ""), a[[1]][[1]])
  obs1  <- c("", "", "", "", "", "", "", paste(names(a[[1]][2]), ":", sep = " ", collapse = ""), a[[1]][[2]])
  ndev <- c("", "", "", "", "", "", "", "", round(summary(x)[[8]], 2))
  resdev <- c("", "", "", "", "", "", "", "", round(summary(x)[[4]], 2))
  logisticPseudoR2s <- function(LogModel) {
    dev <- LogModel$deviance
	  nullDev <- LogModel$null.deviance
  	modelN <-  length(LogModel$fitted.values)
  	R.l <-  1 -  dev / nullDev
  	R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  	R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  	return(c(round(R.l, 3), round(R.cs, 3), round(R.n, 3))) }
  r2s <- logisticPseudoR2s(x)
  R2Nagelkerke <- c("", "","", "", "", "", "", "",round(r2s[[3]], 3))
  R2HosmerLemeshow <- c("", "","", "", "", "", "", "",round(r2s[[1]], 3))
  R2CoxSnell <- c("", "","", "", "", "", "", "",round(r2s[[2]], 3))
  C <- c("", "", "", "", "", "", "", "",round(a[[3]][[6]], 3))
  Dxy <- c("", "", "", "", "", "", "", "",round(a[[3]][[7]], 3))
  AIC <- c("", "", "", "", "", "", "", "", round(summary(x)[[5]], 2))
  Accuracy <- c("", "", "", "", "", "", "", "", paste(round(accuracy, 2), "%", sep = "", collapse = ""))
  ModelLikelihoodRatioTest <- c("", "", "", "", "",
    paste("Model L.R.: ", round(a[[3]][[3]], 2), sep = "", collapse = ""),
    paste("df: ", round(a[[3]][[4]], 0), sep = "", collapse = ""),
    paste("p-value: ", round(a[[3]][[5]], 5), sep = "", collapse = ""),
    paste("sig: ", p.nice(a[[3]][[5]]), sep = "", collapse = ""))
  gblstz.tb <- rbind(mdl.statz, nbcases, obs0, obs1, ndev, resdev,
    R2Nagelkerke, R2HosmerLemeshow, R2CoxSnell, C, Dxy, AIC, Accuracy, ModelLikelihoodRatioTest)
  gblstz.df <- as.data.frame(gblstz.tb)
  colnames(gblstz.df) <- colnames(coef.df)
  blr.tb <- rbind(coef.df, gblstz.df)
  rownames(blr.tb) <- c(rownames(coefs),
    "Model statistics", "Number of cases in model",
    "Observed misses", "Observed successes",
    "Null deviance", "Residual deviance", "R2 (Nagelkerke)", "R2 (Hosmer & Lemeshow)",
    "R2 (Cox & Snell)", "C", "Somers' Dxy", "AIC", "Prediction accuracy", "Model Likelihood Ratio Test")
  blr.df <- as.data.frame(blr.tb)
return(blr.df)
}

