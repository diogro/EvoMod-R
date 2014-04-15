#'No directionsal selection plot
#'
#'Plots a time series of many populations statistics, averagin by genration and treatment
#'@export
NoSelStatMultiPlotMultiPop <- function(drift.list, stab.list, StatMap, y.axis, n.traits = 10){
  data.drift <- laply(drift.list, function (x) StatMap(x$p.cov, 0))
  data.stab <- laply(stab.list, function (x) StatMap(x$p.cov, 0))
  data.drift <- adply(data.drift, 2, function(x) c(mean(x), quantile(x, 0.025), quantile(x, 0.975)))
  data.stab <- adply(data.stab, 2, function(x) c(mean(x), quantile(x, 0.025), quantile(x, 0.975)))
  data.drift[,5] = rep("Drift", length(data.drift))
  data.stab[,5] = rep("Stabilizing", length(data.stab))
  data.avg = data.frame(rbind(data.drift, data.stab))
  data.avg[,1] = as.numeric(levels(data.avg[,1]))[data.avg[,1]]
  names(data.avg) = c("generation", "stat_mean", "stat_lower", "stat_upper", "Selection_scheme")
  time.series  <- ggplot(data.avg, aes(generation, stat_mean, color=Selection_scheme)) +
    geom_smooth(aes(ymin = stat_lower, ymax = stat_upper, color=Selection_scheme), data=data.avg, stat="identity") +
    scale_y_continuous(y.axis)  +
    scale_x_continuous("Generation")
  return(time.series)
}