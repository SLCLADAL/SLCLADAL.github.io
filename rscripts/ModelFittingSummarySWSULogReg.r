###########################################################################
### --- Function for Step-Wise Step-Up Model Fitting Process Summary of Model Comparisons of
### --- (Fixed-Effects) Binomial Logistic Regression Models
###########################################################################
##################################################################
### --- R script "Function for Step-Wise Step-Up Model Fitting Process Summary of Model Comparisons of
### --- (Fixed-Effects) Binomial Logistic Regression Models"
### --- Author: Martin Schweinberger (October 1st, 2014)
### --- R-Version: R version 3.0.1 (2013-05-16) -- "Good Sport"
### --- This R script retrieves relevant information from model comparisons of
### --- Binomial Logistic Regression Models.
### --- NOTE
### --- This script only works for model comparisons of Binomial Logistic Regressions.
### --- The function takes one arguments: a list of model comparisons ordered
### --- according to the progression of model comparisons starting with the
### --- base-line model including only a random effect
### --- The script only works for step-wise step-up model fitting!
### --- The output generated is a table holding relevant model parameters and the
### --- statistic parameters of model comparisons.
### --- CONTACT
### --- If you have questions,suggestions or you found errors
### --- or in case you would to provide feedback, questions
### --- write an email to
### --- martin.schweinberger.hh@gmail.com
### --- CITATION
### --- If you use this script or results thereof, please cite it as:
### --- Schweinberger, Martin. 2014. "Function for Step-Wise Step-Up Model
### --- Fitting Process Summary of Model Comparisons of
### --- Binomial Logistic Regression Models",
### --- unpublished R-skript, Hamburg University.
###############################################################
###                   START
###############################################################

###############################################################
### Function for a neat output table for model comparisons during
### step-wise step-up model fitting
###############################################################
mdl.fttng.swsu.blr <- function(mdlcmps){
mdl.cmp.df1 <- sapply(mdlcmps, function(x) {
  p.nice <- function(z) {
    as.vector(unlist(sapply(z, function(w) {
      ifelse(w < .001, return("p < .001***"),
      ifelse(w < .01, return("p <  .01 **"),
      ifelse(w < .05, return("p <  .05  *"),
      ifelse(w < .1, return("p <  .10(*)"), return("n.s."))))) } ))) }
  hdr <- as.vector(unlist(strsplit(attr(x, "heading"), "\n", fixed = T)))
  hdr <- paste(hdr, collapse = " ")
  hdr <- gsub(" ", "", hdr)
  formula <- gsub(".*Model2:", "", hdr)
  formula.cm <- gsub(".*Model1:", "", hdr)
  formula.cm <- gsub("Model2:.*", "", formula.cm)
  splt.formula <- strsplit(formula, "[~|+|*]")
  splt.formula.cm <- strsplit(formula.cm, "[~|+|*]")
  added <- c(as.vector(unlist(splt.formula)), as.vector(unlist(splt.formula.cm)))
  added <- names(which(table(added)==1))
  added   <- paste(added, collapse = "+")
  added <- gsub("+", "qwertz", added, fixed = T)
  added <- gsub(".*qwertz", "", added)  
  comp    <- formula.cm
  df      <- x[[3]][[2]]
  dev     <- round(x[[4]][[2]], 2)
  p       <- round(x[[5]][[2]], 5)
  sig <-  p.nice(z = p)
  mdl.fttng <- cbind(formula, added, comp, df, dev, p, sig)
}  )
mdl.cmp.df2 <- t(as.data.frame(mdl.cmp.df1))
colnames(mdl.cmp.df2) <- c( "Formula", "Term Added", "Compared to...", "DF", "Residual Deviance", "p-value", "Significance")
mdl.cmp.df2 <- as.data.frame(mdl.cmp.df2)
return(mdl.cmp.df2)
}
###############################################################
### ---                THE END
###############################################################
# TEST FUNCTION        EXAMPLE
#mdlcmp <- list(m1.m0, m2.m1, m3.m2)
# apply function
#test <- mdl.fttng.swsu.blr(mdlcmp)
# inspect results
#test 
