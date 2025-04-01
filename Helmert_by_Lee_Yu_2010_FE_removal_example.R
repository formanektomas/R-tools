# Lee-Yu 2010, Estimation of spatial autoregressive panel data models with FEs
# equaion (5) uses F_{T-1} as follows:
#
#say, T = 7
I7 <- diag(7)
iota7 <- rep(1,7) #
E7 <- iota7 %*% t(iota7) # 7x7 matrix of ones
J7 <- I7 - (E7/7)
#
I7
iota7
E7
J7
#
EIG <- eigen(J7)
EIG$vectors
#
F6 <- EIG$vectors[,1:6] # eigenvectors for the eigenvalue = 1
round(t(F6) %*% F6,5)   # are orthogonal
