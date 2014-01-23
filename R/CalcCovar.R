#'Calculate v-cv matrix
#'
#'Calculate a v-cv matrix usgin a list of correlations and variances
#'@export
CalcCovar  <- function(corr, vars){
  num.gens = length(vars)
  covs = vector('list', num.gens)
  for(i in 1:num.gens)
    covs[[i]] = corr[[i]]*outer(vars[[i]], vars[[i]])
  names(covs) = names(vars)
  return(covs)
}