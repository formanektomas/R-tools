cs <- 2*cos(2*pi*1:500/50 + .6*pi)
w <- rnorm(500,0,1)
par(mfrow=c(2,1), mar=c(3,2,2,1), cex.main=1.5)
plot.ts(cs, main=expression(2*cos(2*pi*t/50+.6*pi))) # beta, gamma, etc. work similarly here.
plot.ts(cs+w, main=expression(2*cos(2*pi*t/50+.6*pi) + N(0,1)))
par(mfrow=c(1,1))
