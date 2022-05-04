# check if notebooks load superfluous packages
knitr::purl("textanalysis.Rmd", documentation = 0)

#install.packages('devtools')
library(devtools)
#install_github("MichaelChirico/funchir")
funchir::stale_package_check('textanalysis.R')