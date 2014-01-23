#'Stat Multi Plot
#'
#'Plots statistics for multiple populations, taking averages

StatMultiPlot <- function(pop.list, StatMap, y.axis, n.traits = 10){
  generation.vector = pop.list[[1]]$generation
  n.gen = length(generation.vector)
  n.pop = length(pop.list)
  data.avg = array(dim=c(n.gen*n.pop, 3))
  for (pop in 1:n.pop){
    stat <- StatMap((pop.list[[pop]]$p.cov))
    print(pop)
    lower = 1+((pop-1)*n.gen)
    upper = pop*n.gen
    label.vector = rep(as.numeric(pop.list[[pop]]$selection.strength), n.gen)
    data.avg[lower:upper,1] = generation.vector
    data.avg[lower:upper,2] = stat
    data.avg[lower:upper,3] = label.vector
  }
  data.avg = data.frame(as.numeric(data.avg[,1]), as.numeric(data.avg[,2]), data.avg[,3])
  names(data.avg) = c("generation", "stat", "Selection_Strength")
  time.series  <- ggplot(data.avg, aes(generation, stat, group = Selection_Strength, color=Selection_Strength)) +
    layer(geom = "smooth") + scale_y_continuous(y.axis)
  return(time.series)
}