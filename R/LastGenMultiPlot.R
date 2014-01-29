#'Last Generation Multi Plor
#'
#'Plots last generation statistics for one populations in relation to selection strenght
#'@export
LastGenStatMultiPlot  <- function(pop.list, StatMap, y.axis, n.traits = 10){
  generation.vector = pop.list[[1]]$generation
  n.gen = length(generation.vector)
  n.pop = length(pop.list)
  data.avg = array(dim=c(n.pop, 2))
  for (pop in 1:n.pop){
    stat <- StatMap(list(pop.list[[pop]]$p.cov[[n.gen]]))
    print(pop)
    lower = pop
    label.vector = as.numeric(pop.list[[pop]]$selection.strength)
    data.avg[pop,1] = stat
    data.avg[pop,2] = label.vector
  }
  data.avg = data.frame(as.numeric(data.avg[,1]), as.numeric(data.avg[,2]))
  names(data.avg) = c("stat", "Selection_Strength")
  time.series  <- ggplot(data.avg, aes(Selection_Strength, stat, group = Selection_Strength)) +
    layer(geom = "boxplot") + scale_y_continuous(y.axis) + scale_x_continuous("Selection Strength")
  return(time.series)
}