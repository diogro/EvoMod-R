#'Caclulate Omega Correlation
#'
#'Calculates the fitness surface correlation with a list of matrices.
#'@export
CalcCorrOmega <- function(mat.list, omega){
  n.traits = dim(mat.list[[1]])[1]
  omega = as.matrix(read.table ("input/omega.csv", header=F, sep=' '))[1:n.traits, 1:n.traits]
  omega = omega[upper.tri(omega)]
  corr.omega <- lapply(mat.list, function(x) cor(x[upper.tri(x)], omega))
  return(unlist(corr.omega))
}