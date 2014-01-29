#'Read mean B matrix
#'
#'Reads mean B matrix from file, return array.
#'@export

ReadBMat <- function(input.file, n.traits, n.loci){
  raw.b = as.numeric(scan(input.file, character()))
  n.gen = sum(raw.b > 1)
  b = raw.b[!raw.b > 1]
  b = array(b, dim = c(n.loci, n.traits, n.gen))
  names(b) = c("loci", "traits", "generation")
  return(b)
}