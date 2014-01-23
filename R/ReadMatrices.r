#' Read Matrices
#'
#'Read a raw matrix file and return a list of matrices
#'@export
ReadMatrices  <- function(input.file, n.traits){
  data.init = read.table(input.file)
  n.corrs = (n.traits*n.traits-n.traits)/2
  gen.number = data.init[seq(1,length(data.init[,1]),n.corrs+1),]
  raw.trait.means = data.init[-seq(1,length(data.init[,1]),n.corrs+1),]
  generations = length(gen.number)
  cor.matrices = vector('list', generations)
  current.mat = matrix(1, n.traits, n.traits)
  for(gen in 1:generations){
    lower = 1+((gen-1)*n.corrs)
    upper = gen*n.corrs
    current.mat[upper.tri(current.mat)] = raw.trait.means[lower:upper]
    current.mat[lower.tri(current.mat)] = t(current.mat)[lower.tri(current.mat)]
    cor.matrices[[gen]] = current.mat
  }
  names(cor.matrices) = gen.number
  return(cor.matrices)
}