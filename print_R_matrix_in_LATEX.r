C <- matrix(1:16,nrow = 4)
#
matrix2latex <- function(matr) {
  printmrow <- function(x) {
    cat(cat(x,sep=" & "),"\\\\ \n")
  }
  cat("\\begin{bmatrix}","\n")
  body <- apply(matr,1,printmrow)
  cat("\\end{bmatrix}")
}
#
matrix2latex(C)
#
# amended from
# https://stackoverflow.com/questions/20749444/converting-r-matrix-into-latex-matrix-in-the-math-or-equation-environment
