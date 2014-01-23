#'Calculate mean statistic
#'
#'alculates a statistic in all directions given a matrix list and a function
#'@export
CalcMeanStat  <- function(mat.list, Stat, nsk = 1000){
  n.traits = dim(mat.list[[1]])[1]
  beta.mat <- array (rnorm (n.traits * nsk), c(n.traits, nsk))
  beta.mat <- apply (beta.mat, 2, Normalize)
  out = lapply(mat.list, function(mat) { return(mean (apply (beta.mat, 2, Stat, cov.matrix = mat)))})
  return(unlist(out))
}