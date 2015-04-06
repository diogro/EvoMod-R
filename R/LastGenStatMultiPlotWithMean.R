#'Last Generation Multi Plor with mean
#'
#'Plots last generation statistics for one populations in relation to selection strenght, both directional and mean
#'@export
LastGenStatMultiPlotWithMean  <- function(pop.list, StatMap, y.axis, n.traits = 10){
  generation.vector = pop.list[[1]]$generation
  n.gen = length(generation.vector)
  MapFunc = function(pop){
    direct.stat <- CalcIsoStat(list(pop$g.cov[[n.gen]]), StatMap)
    mean.stat <- CalcMeanStat(list(pop$g.cov[[n.gen]]), StatMap)
    label.vector = as.numeric(pop$selection.strength)
    return(data.frame(direct.stat, mean.stat, label.vector))
  }
  data.avg = ldply(pop.list, MapFunc, .progress = 'text')
  names(data.avg) = c('.id', "Directional", "Mean", "Selection_Strength")
  data.avg <- melt(data.avg[,-1], id.var = "Selection_Strength")
  data.avg = ddply(data.avg, .(Selection_Strength, variable), 
                function(x) {y <- x$value 
                             c(mean = mean(y), quantile(y, c(0.025, 0.975)))
                })
  names(data.avg) = c('Selection_Strength', 'variable', 'value', 'ymin', 'ymax')
  ggplot(data.avg, aes(Selection_Strength, value, group=interaction(Selection_Strength, variable), color=variable)) + geom_point() + geom_errorbar(aes(ymax = ymax, ymin = ymin)) + scale_y_continuous(y.axis) + scale_x_continuous("Peak Movement Rate")
}