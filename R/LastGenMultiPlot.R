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
  data.avg = ddply(data.avg, .(Selection_Strength), 
                   function(x) {y <- x$stat
                                c(mean = mean(y), quantile(y, c(0.025, 0.975)))
                   })
  names(data.avg) = c('Selection_Strength', 'stat', 'ymin', 'ymax')
  time.series  <- ggplot(data.avg, aes(Selection_Strength, stat, group = Selection_Strength)) +
    geom_point() + geom_errorbar(aes(ymax = ymax, ymin = ymin)) + 
    scale_y_continuous(y.axis) + scale_x_continuous("Peak Movement Rate")
  return(time.series)
}

#'Last Generation Multi Plor
#'
#'Plots last generation statistics for one populations in relation to selection strenght
#'@export
LastGenMultiStatMultiPlot  <- function(pop.list, StatMap, y.axis, n.traits = 10){
  generation.vector = pop.list[[1]]$generation
  n.gen = length(generation.vector)
  n.pop = length(pop.list)
  data.avg = NULL
  for (pop in 1:n.pop){
    stat <- StatMap(list(pop.list[[pop]]$p.cov[[n.gen]]))
    print(pop)
    stat$Selection_Strength = as.numeric(pop.list[[pop]]$selection.strength)
    data.avg = rbind(data.avg, stat)
  }
  data.avg = reshape2::melt(data.avg, id.var = "Selection_Strength")
  data.avg = ddply(data.avg, .(Selection_Strength, variable), 
                   function(x) {y <- x$value 
                                c(mean = mean(y), quantile(y, c(0.025, 0.975)))
                   })
  names(data.avg) = c('Selection_Strength', 'variable', 'value', 'ymin', 'ymax')
  time.series  <- ggplot(data.avg, aes(Selection_Strength, value, group = interaction(Selection_Strength, variable), color = variable)) + geom_point() + geom_errorbar(aes(ymax = ymax, ymin = ymin)) + scale_y_continuous(y.axis) + scale_x_continuous("Peak Movement Rate")
  return(time.series)
}