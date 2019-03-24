# 4-element vector of ones
iota4 <- rep(1,4) #
#
I3 <- diag(3) # I_3 identity matrix
#
kronecker(I3,iota4) # I3 matrix with each row repeated 4 times
kronecker(iota4,I3) # I3 matrix stacked 4-times: 4x1 array of I3
#
J4 <-  iota4 %*% t(iota4) # 4x4 matrix of ones
#
kronecker(I3,J4) # block-diagonal matrix of J4 matrices
kronecker(J4,I3) # I3 matrix stacked: as 4x4 array