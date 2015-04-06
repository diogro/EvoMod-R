#'AVGRatio Plot
#'
#'Plots AVGRatio output
#'@export
#'@import ggplot2 reshape2 plyr
AVGRatioPlot  <- function(pop.list, modules = FALSE, num.cores = 2, x.axis = 'selection.strength', x.label = 'Peak Movement Rate'){
  avg <- ldply(pop.list, function(x) AVGRatio(x$p.cor, x[[x.axis]], num.cores = num.cores), .progress = 'text')
  names(avg)[6] = "Avg_Ratio"
  if(modules){
    m.avg = melt(avg[,-c(2, 3, 6)], id.vars = c('.id', 'Selection_Strength'))
    m.avg = m.avg[!((m.avg['.id'] != "Full Integration") & (m.avg['variable'] == "AVG-")),]
    m.avg = m.avg[!((m.avg['.id'] == "Full Integration") & (m.avg['variable'] == "AVG+")),]
    m.avg = ddply(m.avg, .(Selection_Strength, .id, variable), 
          function(x) {y <- x$value 
                       c(mean = mean(y), quantile(y, c(0.025, 0.975)))
          })
    names(m.avg) = c('Selection_Strength', '.id', 'variable', 'mean', 'ymin', 'ymax')
    avg.plot = ggplot(m.avg, aes(Selection_Strength,
                                 mean,
                                 group=interaction(variable, Selection_Strength, .id),
                                 colour=interaction(.id, variable))) +
      geom_point() + geom_errorbar(aes(ymax = ymax, ymin = ymin)) +
      labs(x=x.label,
           y="Average Correlation",
           color = "Module") +
      scale_colour_discrete(labels=c("Within Module 1",
                                     "Within Module 2",
                                     "Between Modules")) + theme_bw()
  }
  else{
    avg.full = avg[avg['.id'] == "Full Integration",-3]
    avg.full = ddply(avg.full, .(Selection_Strength), 
                     function(x) {y <- x$Avg_Ratio 
                                  c(mean = mean(y), quantile(y, c(0.025, 0.975)))
                     })
    names(avg.full) = c('Selection_Strength', 'mean', 'ymin', 'ymax')
    avg.plot = ggplot(avg.full, aes(Selection_Strength,
                                    mean,
                                    group = Selection_Strength)) + 
      geom_point() + geom_errorbar(aes(ymax = ymax, ymin = ymin)) +
      labs(x="Peak Movement Rate", y="AVG Ratio") + theme_bw()
  }
  return(avg.plot)
}