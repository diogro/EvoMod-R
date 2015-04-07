#' Read original data from dat files
#' 
#' Create R objects of the simulated populations for a single generation
#' @import Matrix
#' @export
ReadRawPop <- function(folder = "/home/diogro/projects/evomod/c-gsl/output/burn_in", label = NULL){
  raw_param = scan(paste0(folder, "/pop.parameters.txt"), character())
  Ne = as.numeric(raw_param[[3]])
  m = as.numeric(raw_param[[9]])
  p = as.numeric(raw_param[[6]])
  rawPop <- array(scan(paste0(folder, "/pop.pop"), numeric()), dim = c(2*m, p+1, Ne))
  names(rawPop) <- list(paste0(c("loci_1-", "loci_2-"), rep(1:m, each = 2)),
                        c("y", paste0("trait", 1:p)),
                        paste0("ind", 1:Ne))
  x <- t(apply(rawPop, 3, function(x) t(x[,-1]) %*% x[,1]))          
  pop = list(y = rawPop[,1,], 
             B = rawPop[,-1,], 
             x = x, 
             G = cov(x), 
             folder = folder, 
             Ne = Ne, 
             m = m, 
             p = p,
             label = label)
  
  class(pop) = "sim_pop"
  return(pop)
}

print.sim_pop <- function(x){
  print(paste("Population Size =", x$Ne))
  print(paste("Number of Loci =", x$m))
  print(paste("Number of traits =", x$p))
  print(paste("Folder =", x$folder))
  if(!is.null(x$label)) print(paste("Label =", x$label)) 
}
