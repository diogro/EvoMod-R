#'CalcR2 wrapper
#'
#'Wrapper for the CalcR2 in a list
#'@export
MapCalcR2  <- function(mat.list){
  r2.list = laply(mat.list, CalcR2)
  return(r2.list)
}