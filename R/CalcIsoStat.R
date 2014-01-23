#'Caculate ISometrics statistic
#'
#'Calculates a statistic in the isometric direction given a matrix list and a function
#'@export
CalcIsoStat  <- function(mat.list, Stat){
  betas = rep(c(1, 0), each=dim(mat.list[[1]])[1]/2)
  betas = Normalize(betas)
  out = laply(mat.list, function(mat) Stat(betas, mat))
  return(out)
}