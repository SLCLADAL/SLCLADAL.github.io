expR <- function(x) {
 ifelse(((ncol(summary(x)$coefficients)-1)/(length(x$fitted)-1)) > .05,
 return(paste("A random sample is expected to cause a correlation of the size",
 ((ncol(summary(x)$coefficients)-1)/(length(x$fitted)-1)),
 "between the predictors and the predicted", collapse = "")),
 return(paste("Based on the sample size expect a false positive correlation of",
 round(((ncol(summary(x)$coefficients)-1)/(length(x$fitted)-1)), 4),
 "between the predictors and the predicted",
 collapse = "")))}