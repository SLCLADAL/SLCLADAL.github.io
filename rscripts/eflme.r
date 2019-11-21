ef.lme <- function(x) {
  df <- summary(x)[[20]][6]
  t <-  summary(x)[[20]][8]
  r <- sqrt((t^2)/((t^2)+df))
  return(paste("Pearson's r = ", round(r, 3)))
}