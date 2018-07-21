# "Manual" logLik calculation for a lm model 
# uncorrelated residuals and normality assumptions apply
#
?swiss # data used
# 
lm1 <- lm(Fertility ~. , data = swiss)
summary(lm1)
logLik(lm1)
#
# uncorrelated residuals assumed
N <- lm1$df.residual + lm1$rank # number of observations used for estimation
p <- lm1$rank  # number of parameters estimated
sigma <- sqrt(sum((lm1$residuals)^2)/(N-p)) * sqrt((N-p)/N)
# 
# log likelihood:
sum(dnorm(swiss$Fertility, mean=fitted(lm1), sd=sigma, log=TRUE))
# alternatively:
sum(dnorm(resid(lm1), mean=0, sd=sigma, log=TRUE))
#
## Amended from 
## https://stats.stackexchange.com/questions/86273/calculate-log-likelihood-by-hand-for-generalized-nonlinear-least-squares-regre
