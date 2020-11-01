## Installing {eurostat} package from GitHub:
##
## Upgrade RStudio and R to latest versions
## Install latest  RTools from https://cran.rstudio.com/bin/windows/Rtools/
## Run the following code in R
##
## Install devtools - it will allow you to install packages from GitHub
install.packages("devtools")
## activate the package
library(devtools)
## install eurostat
install_github("rOpenGov/eurostat")
## there may be a prompt asking you to install other supporting packages, choose 1. All
