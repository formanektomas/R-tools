# Plot individual series & corresponding prediction for an estimated VAR model
# .. using autoplot() from the {ggplot2} library
#
# example based on "Canada" sample dataset from the {vars} package
#
#
library(vars)
library(ggplot2)
#
data(Canada)
var1 <- VAR(Canada, p = 2, type = "none")
#
fc1 <- forecast(var1) # default settings
autoplot(fc1)
#
fc1$forecast
fc1$forecast$U
#
autoplot(fc1$forecast$U)
#
