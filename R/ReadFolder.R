#'Read Folder
#'
#'Read a single population folder and return a list with all poplation atributes
#'@export
ReadFolder  <- function(input.folder, n.traits = 10, sel.type, direct.sel = T){
  print(input.folder)
  input.folder = paste("./output", input.folder, sep="/")
  input.file = paste(input.folder, "p.corr.dat", sep = '/')
  p.cor = ReadMatrices(input.file, n.traits)
  input.file = paste(input.folder, "g.corr.dat", sep = '/')
  g.cor = ReadMatrices(input.file, n.traits)
  input.file = paste(input.folder, "p.var.dat", sep = '/')
  p.var = ReadVariances(input.file, n.traits)
  input.file = paste(input.folder, "g.var.dat", sep = '/')
  g.var = ReadVariances(input.file, n.traits)
  input.file = paste(input.folder, "h.var.dat", sep = '/')
  h.var = ReadVariances(input.file, n.traits)
  input.file = paste(input.folder, "phenotype.dat", sep = '/')
  phenotype = ReadVariances(input.file, n.traits)
  input.file = paste(input.folder, "b.mat.dat", sep = '/')
  if(file.exists(input.file))
    b.mat = ReadBMat(input.file, n.traits, n.loci = 2*500)
  else
    b.mat = NULL
                   
  p.cov = CalcCovar(p.cor, p.var)
  g.cov = CalcCovar(g.cor, g.var)
  
  if(direct.sel){
    aux.file = paste(input.folder, "pop.parameters.txt", sep="/")
    parameters = scan(aux.file, character(), quiet = TRUE)
    index = which("theta"==parameters)+2
    selection.strength = as.numeric(parameters[index])
    index = which("Ne"==parameters)+2
    n.e = as.numeric(parameters[index])
    index = which("mu_b"==parameters)+2
    mu_b = as.numeric(parameters[index])
  }
  else{
    selection.strength = 0.
  }
  out.list = list(p.cor = p.cor,
                  g.cor = g.cor,
                  p.var = p.var,
                  g.var = g.var,
                  h.var = h.var,
                  p.cov = p.cov,
                  g.cov = g.cov,
                  b.mat = b.mat,
                  selection.type = sel.type,
                  selection.strength = selection.strength,
                  generation = as.numeric(names(p.var)))
  return(out.list)
}