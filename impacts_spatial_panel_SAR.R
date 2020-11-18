# As of November 18, 2020, impacts() functions does not work in splm package
# - function not in {splm} and panels not supported by {spatialreg}
# - however, spdep::impacts stil can do the job:
# Example amended from ?spml in the {splm} library
#
library(splm) #
data(Produc, package = "plm")
data(usaww)
fm <- log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp
## the two standard specifications (SEM and SAR) one with FE
respatlag <- spml(fm, data = Produc, listw = mat2listw(usaww),
                  model="random", spatial.error="none", lag=TRUE)
summary(respatlag)
## calculate impact measures
impac1 <- spdep::impacts(respatlag, listw = mat2listw(usaww, style = "W"), time = 17) # here: spdep::impacts  
summary(impac1, zstats=TRUE, short=TRUE)
