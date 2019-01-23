##########################################################################
### --- R script "Pseudo R2^for Binomial Logistic Mixed-Effects Models"
### --- Source:
### --- http://hlplab.wordpress.com/2009/08/29/nagelkerke-and-coxsnell-pseudo-r2-for-mixed-logit-models/#more-564
##########################################################################

# Input:
# The function expects a vector of two character strings as first
# argument, which should be the fixed-effect part of the formula
# (i.e. “dv ~ factorA + factorB”) and the random effect part (e.g. “(1 | Subject)“).
# For example, the first argument might be c(“dv ~ a + b”, “(1 + a | Subject)”).
#  The second argument should be the data set on which the model was fit
# (i.e. make sure that it’s only and all of those cases that went into the model fit).
# NB:
# The data set should only contain cases for which all predictors of the
# full model are defined.
#Output:
# The function provides CoxSnell and Nagelkerke R-squares for the full
# model compared against two baseline models, (1) a mixed logit model with only
# the intercept and the random effects and (2) an ordinary logit model with only
# the intercept. It also provides (3) the R-square measures for (1) compared
# against the baseline model (2).
# This can be seen as providing pseudo measures of the variance accounted for
# by the fixed effects compare to (1) the baseline mixed model and compared (2)
# the ordinary baseline model, while at the same providing a measure of how
# much the random effects account for compared to the ordinary baseline model.

my.lmer.nagelkerke <- function(f, d) {
 lmer.full= lmer(formula= as.formula(paste(f, collapse="+")), d, family="binomial")
 logLik.lmer.full= as.numeric(logLik(lmer.full))
 N.lmer.full= nrow(lmer.full@X)
 cat(paste("Full mixed model: L=", logLik.lmer.full, ", N=", N.lmer.full, "\n", sep=""))

 lmer.intercept= lmer(formula= as.formula(paste(unlist(strsplit(f[1], "~"))[1], paste("1", f[2], sep=" + "), sep="~ ")), data= d, family="binomial")
 logLik.lmer.intercept= as.numeric(logLik(lmer.intercept))
 N.lmer.intercept= nrow(lmer.intercept@X)
 cat(paste("Intercept mixed model: L=", logLik.lmer.intercept, ", N=", N.lmer.intercept, "\n", sep=""))

 lrm.full= lrm(formula= as.formula(f[1]), data= d)
 logLik.lrm.intercept= as.numeric(deviance(lrm.full)[1] / - 2)
 N.lrm.intercept= as.numeric(lrm.full$stats[1])
 cat(paste("Intercept ordinary model: L=", logLik.lrm.intercept, ", N=", N.lrm.intercept, "\n", sep=""))

 coxsnell.lmer= 1 - exp((logLik.lmer.intercept - logLik.lmer.full) * (2/N.lmer.full))
 nagelkerke.lmer= coxsnell.lmer / (1 - exp(logLik.lmer.intercept * (2/N.lmer.full)))
 cat(paste("Full model evaluated against mixed intercept model:\n\tCoxSnell R2: ", coxsnell.lmer, "\n\tNagelkerke R2: ", nagelkerke.lmer,"\n", sep=""))
 coxsnell.lrm= 1 - exp((logLik.lrm.intercept - logLik.lmer.full) * (2/N.lmer.full))
 nagelkerke.lrm= coxsnell.lrm / (1 - exp(logLik.lrm.intercept * (2/N.lmer.full)))
 cat(paste("Full model evaluated against ordinary intercept model:\n\tCoxSnell R2: ", coxsnell.lrm, "\n\tNagelkerke R2: ", nagelkerke.lrm,"\n", sep=""))
 coxsnell.lrm.2= 1 - exp((logLik.lrm.intercept - logLik.lmer.intercept) * (2/N.lmer.full))
 nagelkerke.lrm.2= coxsnell.lrm.2 / (1 - exp(logLik.lrm.intercept * (2/N.lmer.full)))
 cat(paste("Mixed intercept model evaluated against ordinary intercept model:\n\tCoxSnell R2: ", coxsnell.lrm.2, "\n\tNagelkerke R2: ", nagelkerke.lrm.2,"\n", sep=""))
}


