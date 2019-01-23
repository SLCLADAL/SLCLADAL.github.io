smplesz <- function(x) {
 ifelse((length(x$fitted)<(104 + ncol(summary(x)$coefficients)-1)) == TRUE,
 return(
 paste("Sample too small: please increase your sample by ",
 104 + ncol(summary(x)$coefficients)-1 - length(x$fitted),
 " data points", collapse = "")),
 return("Sample size sufficient")) }