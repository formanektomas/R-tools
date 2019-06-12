### https://en.wikipedia.org/wiki/RV_coefficient
### https://stackoverflow.com/questions/4034271/pairwise-matrix-correlations-in-r-how-to-iterate-through-all-pairs
#
Rv <- function(M1, M2) {  
  tr <- function(x) sum( diag(x) )   
  psd <- function(x) x %*% t(x)   
  AA <- psd(M1)  
  BB <- psd(M2)  
  num <- tr(AA %*% BB)  
  den <- sqrt( tr(AA %*% AA) * tr(BB %*% BB) )  
  Rv <- num / den  
  list(Rv=Rv, "Rv^2"=Rv^2)  
}  
#
### Sample RV calculation
#
matrix1 <- matrix(rnorm(100), 10, 10)  
matrix2 <- matrix(rnorm(100), 10, 10)  
#
Rv(matrix1, matrix2)  
