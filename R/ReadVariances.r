#'Read Variances
#'
#'Read a raw variance or phenotype file and return a list
#'@export
ReadVariances  <- function(input.file, n.traits){
  data.init = read.table(input.file)
  gen.number = data.init[seq(1,length(data.init[,1]),n.traits+1),]
  raw.trait.means = data.init[-seq(1,length(data.init[,1]),n.traits+1),]
  generations = length(gen.number)
  var.vectors = vector('list', generations)
  for(gen in 1:generations){
    lower = 1+((gen-1)*n.traits)
    upper = gen*n.traits
    raw.trait.means[lower:upper]
    var.vectors[[gen]] = raw.trait.means[lower:upper]
  }
  names(var.vectors) = gen.number
  return(var.vectors)
}