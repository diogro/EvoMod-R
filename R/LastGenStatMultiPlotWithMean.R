#'Last Generation Multi Plor with mean
#'
#'Plots last generation statistics for one populations in relation to selection strenght, both directional and mean
#'@export
LastGenStatMultiPlotWithMean  <- function(pop.list, StatMap, y.axis, n.traits = 10){
  generation.vector = pop.list[[1]]$generation
  n.gen = length(generation.vector)
  n.pop = length(pop.list)
  data.avg = array(dim=c(n.pop, 3))
  for (pop in 1:n.pop){
    direct.stat <- CalcIsoStat(list(pop.list[[pop]]$p.cov[[n.gen]]), StatMap)
    mean.stat <- CalcMeanStat(list(pop.list[[pop]]$p.cov[[n.gen]]), StatMap)
    print(pop)
    lower = pop
    label.vector = as.numeric(pop.list[[pop]]$selection.strength)
    data.avg[pop,1] = direct.stat
    data.avg[pop,2] = mean.stat
    data.avg[pop,3] = label.vector
  }
  data.avg = data.frame(as.numeric(data.avg[,1]), as.numeric(data.avg[,2]), as.numeric(data.avg[,3]))
  names(data.avg) = c("Directional", "Mean", "Selection_Strength")
  data.avg = melt(data.avg, c("Selection_Strength"))
  time.series  <- ggplot(data.avg, aes(Selection_Strength, value, group=interaction(Selection_Strength, variable),color=variable)) +
    layer(geom = "boxplot") + scale_y_continuous(y.axis) + scale_x_continuous("Selection Strength")
  return(time.series)
}