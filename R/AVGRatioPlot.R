#'AVGRatio Plot
#'
#'Plots AVGRatio output
#'@export
#'@import ggplot2 reshape2 plyr
AVGRatioPlot  <- function(pop.list, modules = FALSE, num.cores = 2){
  avg <- ldply(pop.list, function(x) AVGRatio(x$p.cor, x$selection.strength, num.cores = num.cores), .progress = 'text')
  names(avg)[6] = "Avg_Ratio"
  if(modules){
    m.avg = melt(avg[,-c(2, 3, 6)], id.vars = c('.id', 'Selection_Strength'))
    m.avg = m.avg[!((m.avg['.id'] != "Full Integration") & (m.avg['variable'] == "AVG-")),]
    m.avg = m.avg[!((m.avg['.id'] == "Full Integration") & (m.avg['variable'] == "AVG+")),]
    avg.plot = ggplot(m.avg, aes(Selection_Strength,
                                 value,
                                 group=interaction(variable, Selection_Strength, .id),
                                 colour=interaction(.id, variable))) +
      layer(geom="boxplot") +
      labs(x="Selection Strength",
           y="Average Correlation",
           color = "Module") +
      scale_colour_discrete(labels=c("Within Module 1",
                                     "Within Module 2",
                                     "Between Modules")) + theme_bw()
  }
  else{
    avg.full = avg[avg['.id'] == "Full Integration",-3]
    avg.plot = ggplot(avg.full, aes(Selection_Strength,
                                    Avg_Ratio,
                                    group = Selection_Strength)) + layer(geom="boxplot") +
      labs(x="Selection Strength", y="AVG Ratio") + theme_bw()
  }
  return(avg.plot)
}