###########################################################################
### --- Function for Customized Mixed-Effects Binomial Logistic Regression Results
###########################################################################
##################################################################
### --- R script "Function for Mixed-Effects Customized Binomial Logistic Regression Results"
### --- Author: Martin Schweinberger (August 18th, 2014)
### --- R-Version: R version 3.0.1 (2013-05-16) -- "Good Sport"
### --- This R script retrieves relevant information from regression outputs of
### --- Binomial Logistic Regression.
### --- NOTE
### --- This script only works for Mixed-Effects Binomial Logistic Regressions.
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
# glm0 = glm object with only an intercept, glm1 = glm object with same fixed effects as final glmer object,
# glmer0 = glmer object with only a random effect, glmer1 = final glmer model, 
# dpvar =  vector with 0 (no success/no hit) or 1 (success/hit), i.e the dependent
# variable of the final data set without any missing values
###############################################################
### --- Example
#glm0 <-  glm(suf.LIKE ~ 1, family = binomial, data = newdata) # baseline model glm
#glm1 <-  glm(suf.LIKE ~ age * ed.level, family = binomial, data = newdata) # final glm model
#glmer0 <- glmer(suf.LIKE ~ (1|file.subfile.spk), family = binomial, data = newdata, control = glmerControl(optimizer = "Nelder_Mead", calc.derivs = F))
#glmer1 <- glmer(suf.LIKE ~ (1|file.subfile.spk) + log.len.like + age * ed.level, family = binomial, data = newdata, control = glmerControl(optimizer = "Nelder_Mead", calc.derivs = F))
#dpvar <- mydata$suf.LIKE

###############################################################

meblrm.summary <- function(glm0, glm1, glmer0, glmer1, dpvar) {
  p.nice <- function(z) {
    as.vector(unlist(sapply(z, function(w) {
      ifelse(w < .001, return("p < .001***"),
      ifelse(w < .01, return("p <  .01 **"),
      ifelse(w < .05, return("p <  .05  *"), 
      ifelse(w < .1, return("p <  .10(*)"), return("n.s."))))) } ))) }
  
  LLglm0 <- logLik(glm0)
  LLglmer0 <- logLik(glmer0)
  LLR01 <- as.vector(- 2 * (LLglm0 - LLglmer0))
  df <- attr(LLglmer0, "df") - attr(LLglm0, "df")
  p <- pchisq(LLR01, df, lower.tail = FALSE)
  headranef <- c("Group(s)", "Variance", "Std. Dev.", " ", "  ", "L.R. X2", "DF", "Pr", "Significance")
  ranef <- c(names(summary(glmer1)[[9]]), round(summary(glmer1)[[13]][[1]][[1]],2), 
  round(as.data.frame(VarCorr(glmer1))[[5]], 2), 
    "", "", round(LLR01, 2), df, round(p, 4), p.nice(p))

# take vif-mer function from https://github.com/aufrank/R-hacks/blob/master/mer-utils.R on 14th August, 2014
vif.mer <- function (fit) {
  ## adapted from rms::vif
  v <- vcov(fit)
  nam <- names(fixef(fit))
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
  v <- v[-(1:ns), -(1:ns), drop = FALSE]
  nam <- nam[-(1:ns)]
  }
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}
kappa.mer <- function (fit, scale = TRUE, center = FALSE,
  add.intercept = TRUE, exact = FALSE) {
  X <- fit@X
  nam <- names(fixef(fit))
  ## exclude intercepts
  nrp <- sum(1 * (nam == "(Intercept)"))
  if (nrp > 0) {
    X <- X[, -(1:nrp), drop = FALSE]
    nam <- nam[-(1:nrp)]
    }
  if (add.intercept) {
    X <- cbind(rep(1), scale(X, scale = scale, center = center))
    kappa(X, exact = exact)
    } else {
    kappa(scale(X, scale = scale, center = scale), exact = exact)
  }
}
colldiag.mer <- function (fit, scale = TRUE, center = FALSE,
  add.intercept = TRUE) {
  ## adapted from perturb::colldiag, method in Belsley, Kuh, and
  ## Welsch (1980). look for a high condition index (> 30) with
  ## more than one high variance propotion. see ?colldiag for more
  ## tips.
  result <- NULL
  if (center)
  add.intercept <- FALSE
  if (is.matrix(fit) || is.data.frame(fit)) {
    X <- as.matrix(fit)
    nms <- colnames(fit)
    }
  else if (class(fit) == "mer") {
    nms <- names(fixef(fit))
    X <- fit@X
      if (any(grepl("(Intercept)", nms))) {
      add.intercept <- FALSE
    }
  }
  X <- X[!is.na(apply(X, 1, all)), ]
  if (add.intercept) {
    X <- cbind(1, X)
    colnames(X)[1] <- "(Intercept)"
    }
  X <- scale(X, scale = scale, center = center)
  svdX <- svd(X)
  svdX$d
  condindx <- max(svdX$d)/svdX$d
  dim(condindx) <- c(length(condindx), 1)
  Phi = svdX$v %*% diag(1/svdX$d)
  Phi <- t(Phi^2)
  pi <- prop.table(Phi, 2)
  colnames(condindx) <- "cond.index"
  if (!is.null(nms)) {
    rownames(condindx) <- nms
    colnames(pi) <- nms
    rownames(pi) <- nms
  } else {
    rownames(condindx) <- 1:length(condindx)
    colnames(pi) <- 1:ncol(pi)
    rownames(pi) <- 1:nrow(pi)
  }
  result <- data.frame(cbind(condindx, pi))
  zapsmall(result)
}
maxcorr.mer <- function (fit, exclude.intercept = TRUE) {
  so <- summary(fit)
  corF <- so@vcov@factors$correlation
  nam <- names(fixef(fit))
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0 & exclude.intercept) {
    corF <- corF[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  corF[!lower.tri(corF)] <- 0
  maxCor <- max(corF)
  minCor <- min(corF)
  if (abs(maxCor) > abs(minCor)) {
    zapsmall(maxCor)
  } else {
    zapsmall(minCor)
  }
}

# continue with setting up table
  coefs <- summary(glmer1)[[10]]

  se <- sqrt(diag(vcov(glmer1)))
  cilwr <- fixef(glmer1) - 1.96 * se
  ciupr <- fixef(glmer1) + 1.96 * se
  coef.df <- data.frame(
    round(coefs[, 1], 2),
    c("", round(vif.mer(glmer1), 2)),
    round(exp(coefs[, 1]), 2),
    round(exp(cilwr), 2),
    round(exp(ciupr), 2),
    round(coefs[, 2], 2),
    round(coefs[, 3], 2),
    round(coefs[, 4], 4),
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
    
  coef.df <- rbind(colnames(coef.df), coef.df)
  coef.df <- as.data.frame(coef.df)
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
  groups <- c("", "", "", "", "", "", "", "", summary(glmer1)[[9]][[1]])
  nbcases <- c("", "", "", "", "", "", "", "", length(fitted(glmer1)))
  obs0 <- c("", "", "", "", "", "", "", "", sum(dpvar == 0))
  obs1  <- c("", "", "", "", "", "", "", "", sum(dpvar == 1))
  resdev <- c("", "", "", "", "", "", "", "", round(summary(glmer1)[[3]][[1]][[8]], 2))

  logisticPseudoR2s <- function(glm0, glmer0, glmer1) {
    dev <- deviance(glmer1)
    nullDev <- deviance(glmer0)
    modelN <-  length(fitted(glmer1))
    
    nullDev.glm <- glm0$null.deviance
    R.l.glm <-  1-dev/nullDev.glm
    R.cs.glm <- 1-exp(-(nullDev.glm-dev)/modelN)
    R.n.glm <- R.cs.glm/(1-(exp(-(nullDev.glm/modelN))))
    
    return(c(R.l.glm,      # Hosmer and Lemeshow R^2
      R.cs.glm,            # Cox and Snell R^2
      R.n.glm))            # Nagelkerke R^2
  }

  r2s <- logisticPseudoR2s(glm0, glmer0, glmer1)
  R2Nagelkerke <- c("", "","", "", "", "", "", "",round(r2s[[3]], 3))
  R2HosmerLemeshow <- c("", "","", "", "", "", "", "",round(r2s[[1]], 3))
  R2CoxSnell <- c("", "","", "", "", "", "", "",round(r2s[[2]], 3))

  probs <- 1/(1+exp(-fitted(glmer1)))
  #modstatz <- somers2(probs, as.numeric(dpvar))
  modstatz <- somers2(probs, as.numeric(dpvar)-1)
  C <- c("", "", "", "", "", "", "", "",round(modstatz[[1]], 3))
  Dxy <- c("", "", "", "", "", "", "", "",round(modstatz[[2]], 3))
  AIC <- c("", "", "", "", "", "", "", "", round(summary(glmer1)[[14]][[1]], 2))
  BIC <- c("", "", "", "", "", "", "", "", round(summary(glmer1)[[14]][[2]], 2))


  dpvarneg <- sapply(dpvar, function(x) ifelse(x == 1, 0, 1))
  correct <- sum(dpvar * (predict(glmer1, type = "response") >= 0.5)) + sum(dpvarneg * (predict(glmer1, type="response") < 0.5))
  tot <- length(dpvar)
  predict.acc <- (correct/tot)*100 

  Accuracy <- c("", "", "", "", "", "", "", "", paste(round(predict.acc, 2), "%", sep = "", collapse = ""))
  
  L0 <- logLik(glm0)
  L1 <- logLik(glmer1)
  L01 <- as.vector(- 2 * (L0 - L1))
  df <- attr(L1, "df") - attr(L0, "df")
    
  ModelLikelihoodRatioTest <- c("", "", "", "", "",
    paste("L.R. X2: ", round(L01, 2), sep = "", collapse = ""),
    paste("DF: ", df, sep = "", collapse = ""),
    paste("p-value: ", round(pchisq(L01, df, lower.tail = FALSE), 5), sep = "", collapse = ""),
    paste("sig: ", p.nice(pchisq(L01, df, lower.tail = FALSE)), sep = "", collapse = ""))

  ranef.tb <- rbind(ranef)
  ranef.df <- as.data.frame(ranef.tb)
  colnames(ranef.df) <- colnames(coef.df)
    
  gblstz.tb <- rbind(mdl.statz, groups, nbcases, obs0, obs1, resdev,
    R2Nagelkerke, R2HosmerLemeshow, R2CoxSnell, C, Dxy, AIC, BIC, Accuracy, ModelLikelihoodRatioTest)
  gblstz.df <- as.data.frame(gblstz.tb)
  colnames(gblstz.df) <- colnames(coef.df)

  blr.tb <- rbind(ranef.df, coef.df, gblstz.df)
  colnames(blr.tb) <- headranef
  rownames(blr.tb) <- c("Random Effect(s)", "Fixed Effect(s)", rownames(coefs),
    "Model statistics", "Number of Groups", "Number of cases in model", 
    "Observed misses", "Observed successes",
    "Residual deviance", "R2 (Nagelkerke)", "R2 (Hosmer & Lemeshow)",
    "R2 (Cox & Snell)", "C", "Somers' Dxy", "AIC", "BIC", "Prediction accuracy", "Model Likelihood Ratio Test")
  blr.df <- as.data.frame(blr.tb)
return(blr.df)
}

#meblrm.summary(glm0, glm1, glmer0, glmer1, dpvar)
