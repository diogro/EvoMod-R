#' Time series mantel
#' 
#' Plots sequencial mantel comparisons alog generations
#' @export
TimeSeriesMantel <- function(cor.list, num.cores = 4){
    if (num.cores > 1) {
        library(doMC)
        library(foreach)
        registerDoMC(num.cores)
        parallel = TRUE
    }
    else{
        parallel = FALSE
    }
    n.gen = length(cor.list)
    comparisons <- aaply(1:(n.gen-1), 1, function(x) MantelCor(cor.list[[x]], cor.list[[x+1]], iterations = 1)[1])
    return(comparisons)
}
