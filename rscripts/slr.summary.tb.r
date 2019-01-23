slr.summary <- function(x) {
p.nice <- function(z) {
  as.vector(unlist(sapply(z, function(w) {
    ifelse(w < .001, return("p < .001***"),
    ifelse(w < .01, return("p < .01**"),
    ifelse(w < .05, return("p < .05*"), return(w)))) } ))) }
  intercept <- c(
    round(summary(x)[[4]][1], 2),
    "", "",
    round(summary(x)[[4]][3], 2),
    round(summary(x)[[4]][5], 2),
    round(summary(x)[[4]][7], 4),
    p.nice(summary(x)[[4]][7]))
  predictor <- c(
    round(summary(x)[[4]][2], 2),
    round(lm.beta(x)[[1]], 4),
    round(sqrt(summary(x)[[8]]), 2),
    round(summary(x)[[4]][4], 2),
    round(summary(x)[[4]][6], 2),
    round(summary(x)[[4]][8], 4),
    p.nice(summary(x)[[4]][8]))
  mdl.statz <- c("", "", "", "", "", "", "Value")
  nbcases <- c("", "", "", "", "", "", length(summary(x)[[3]]))
  rse <- c("", "", "", "", "", "", round(summary(x)[[6]], 2))
  multR2 <- c("", "", "", "", "", "", round(summary(x)[[8]], 4))
  adjR2 <- c("", "", "", "", "", "", round(summary(x)[[9]], 4))
  F <- c("", "", "", "", "", "",
    round(summary(x)[[10]][1], 2))
  p <- c("", "", "", "", "", "", round(summary(x)[[4]][8], 4))
  slrm.tb <- rbind(intercept, predictor, mdl.statz, nbcases, rse, multR2, adjR2, F, p )
  colnames(slrm.tb) <- c(colnames(summary(x)[[4]])[1], "Std. Beta",  "Pearson's r",
  colnames(summary(x)[[4]])[c(2:4)], "P-value sig.")
  rownames(slrm.tb) <- c(
    rownames(summary(x)[[4]])[1],
    rownames(summary(x)[[4]])[2],
    "Model statistics", "Number of cases in model",
    paste("Residual standard error", paste("on", summary(x)[[7]][2],"DF")),
    "Multiple R-squared", "Adjusted R-squared",
  paste("F-statistic",
    paste("(", round(summary(x)[[10]][2], 0), ", ",
    round(summary(x)[[10]][3], 0), ")", sep = "", collapse = "")), "Model p-value")
  slrm.tb <- as.data.frame(slrm.tb)
return(slrm.tb)
}