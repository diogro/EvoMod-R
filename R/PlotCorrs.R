#'Plot Correlations
#'
#'Plot within module average correlations
#'@export
PlotCorrs = function(p.cor, generations = 1: length(p.cor)){
  burn.in.avg = AVGRatio(p.cor, 40/10000, F, num.cores = 10, generations = generations)
  burn.in.avg['generation'] = burn.in.avg['generation']
  burn.in.avg['Probability'] = NULL
  burn.in.avg['Selection_Strength'] = NULL
  m.avg = melt(burn.in.avg[,-c(2,5)], id.vars = c('.id', 'generation'))
  m.avg = m.avg[!((m.avg['.id'] != "Full Integration") & (m.avg['variable'] == "AVG-")),]
  m.avg = m.avg[!((m.avg['.id'] == "Full Integration") & (m.avg['variable'] == "AVG+")),]
  avg.plot = ggplot(m.avg, aes(generation,
                               value,
                               group=interaction(variable, generation, .id),
                               colour=interaction(.id, variable))) +
    layer(geom="point") +
    labs(x="Generation",
         y="Average Correlation",
         color = "Module") +
    scale_colour_discrete(labels=c("Within Module 1",
                                   "Within Module 2",
                                   "Between Modules")) + theme_bw()
  return(avg.plot)
}