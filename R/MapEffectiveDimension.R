#'Effective dimenson wrapper
#'
#'Wrapper to calculate effective dimension in a list
#'@export
MapEffectiveDimension <- function(mat.list){
  nd.list <- lapply(mat.list, function(x){
    eVals  <-  eigen(x)$values
    return(sum(eVals)/eVals[1])
  })
  return(unlist(nd.list))
}