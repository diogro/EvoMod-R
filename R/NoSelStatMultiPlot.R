#'No directionsal selection plot
#'
#'Plots a time series of many populations statistics, averagin by genration
#'@export

NoSelStatMultiPlot <- function(pop.list, StatMap, y.axis, n.traits = 10){
  data.avg <- laply(pop.list, function (x) StatMap(x$p.cov))
  data.avg <- adply(data.avg, 2, function(x) c(mean(x), quantile(x, 0.025), quantile(x, 0.975)))
  data.avg[,1] = as.numeric(levels(data.avg[,1]))[data.avg[,1]]
  names(data.avg) = c("generation", "stat_mean", "stat_lower", "stat_upper")
  time.series  <- ggplot(data.avg, aes(generation, stat_mean)) +
    geom_smooth(aes(ymin = stat_lower, ymax = stat_upper), data=data.avg, stat="identity") +
    scale_y_continuous(y.axis)  +
    scale_x_continuous("Generation")
  return(time.series)
}