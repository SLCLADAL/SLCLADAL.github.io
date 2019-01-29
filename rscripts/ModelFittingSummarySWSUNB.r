###########################################################################
### --- Function for Step-Wise Step-Up Model Fitting Process Summary of Model Comparisons of
### --- Mixed-Effects Binomial Logistic Regression Models
###########################################################################
##################################################################
### --- R script "Function for Step-Wise Step-Up Model Fitting Process Summary of Model Comparisons of
### --- Negative Binomial Regression Models"
### --- Author: Martin Schweinberger (April 21st, 2015)
### --- R-Version: R version 3.0.1 (2013-05-16) -- "Good Sport"
### --- This R script retrieves relevant information from model comparisons of
### --- Negative Binomial Regression Models.
### --- NOTE
### --- This script only works for model comparisons of Negative Binomial Regression Models.
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
### --- Schweinberger, Martin. 2015. "Function for Step-Wise Step-Up Model
### --- Fitting Process Summary of Model Comparisons of
### --- Negative Binomial Regression Models",
### --- unpublished R-skript, Hamburg University.
###############################################################
###                   START
###############################################################

###############################################################
### Function for a neat output table for model comparisons during step-wise step-up model fitting
###############################################################
# glm0 = glm object with only an intercept, glm1 = glm object with same fixed effects as final glmer object,
# glmer0 = glmer object with only a random effect, glmer1 = final glmer model,
# dpvar =  vector with 0 (no success/no hit) or 1 (success/hit), i.e the dependent
# variable of the final data set without any missing values
###############################################################
### ---               START
###############################################################
### create table to report model fitting process
mdl.fttng.nb.swsu <- function(mdlcmp){
mdl.cmp.df1 <- sapply(mdlcmp, function(x) {
  p.nice <- function(z) {
    as.vector(unlist(sapply(z, function(w) {
      ifelse(w < .001, return("p<.001***"),
      ifelse(w < .01, return("p<.01 **"),
      ifelse(w < .05, return("p<.05  *"),
      ifelse(w < .1, return("p<.10(*)"), return("n.s."))))) } ))) }
  add <- gsub("\n", " ", x$Model, fixed = T)
  add <- gsub(" {2,}", " ", add)
  add <- str_trim(add, side = "both")
  add <- gsub(add[1], "", add[2], fixed = T)
  add <- gsub(".* ", "", add)
  df      <- x[[3]][[2]]
  aic     <- round(x[[4]][[2]], 2)
  x2      <- round(x[[7]][[2]], 2)
  x2df    <- x[[6]][[2]]
  p       <- round(x[[8]][[2]], 5)
  sig <-  p.nice(z = p)
  mdl.fttng <- cbind(add, df, aic, x2, x2df, p, sig)
}  )
mdl.cmp.df2 <- t(mdl.cmp.df1)
colnames(mdl.cmp.df2) <- c("Term Added", "DF", "AIC", "X2", "X2DF", "p-value", "Significance")
mdl.cmp.df2 <- as.data.frame(mdl.cmp.df2)
return(mdl.cmp.df2)
}
###############################################################
### ---                 THE END
###############################################################
# TEST FUNCTION
# EXAMPLE
#mdlcmp <- list(m1.m0, m2.m1, m3.m2)
# apply function
#test <- mdl.fttng.swsu(mdlcmp)
# inspect results
#test

