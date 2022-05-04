# check if notebooks load superfluous packages
knitr::purl("basicstatz.Rmd", documentation = 0)

#install.packages('devtools')
library(devtools)
#install_github("MichaelChirico/funchir")
funchir::stale_package_check('basicstatz.R')