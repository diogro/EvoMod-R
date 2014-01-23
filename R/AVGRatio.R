#'AVG Ratio Calculation
#'
#'Calculate the AVG ratio for a list of matrices, returning a data.frame
#'@export
AVGRatio <- function(mat.list, Selection_Strength, last.gen = T, num.cores = 1, generations = 1:10000 + 20000){
  if (num.cores > 1) {
    library(doMC)
    library(foreach)
    registerDoMC(num.cores)
    parallel = TRUE
  }
  else{
    parallel = FALSE
  }
  n.traits = dim(mat.list[[1]])[1]
  module.1 = rep(c(1,0), each = n.traits/2)
  module.2 =  rep(c(0,1), each = n.traits/2)
  modularity.hipot = cbind(module.1, module.2)
  if(last.gen)
    AVGRatio <- ldply(mat.list[length(mat.list)], function(x) TestModularity(x, modularity.hipot, iterations=0), .parallel = parallel)
  else{
    AVGRatio <- ldply(mat.list, function(x) TestModularity(x, modularity.hipot, iterations=0), .parallel = parallel)
    AVGRatio['generation'] = rep(generations, each = 3)
  }
  AVGRatio['Selection_Strength'] = Selection_Strength
  return(AVGRatio)
}