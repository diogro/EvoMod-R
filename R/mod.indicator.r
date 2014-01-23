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

LastGenStatMultiPlotWithMean  <- function(pop.list, StatMap, y.axis, n.traits = 10){
    generation.vector = pop.list[[1]]$generation
    n.gen = length(generation.vector)
    n.pop = length(pop.list)
    data.avg = array(dim=c(n.pop, 3))
    for (pop in 1:n.pop){
        direct.stat <- CalcIsoStatMap(list(pop.list[[pop]]$p.cov[[n.gen]]), StatMap)
        mean.stat <- CalcMeanStatMap(list(pop.list[[pop]]$p.cov[[n.gen]]), StatMap)
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

NoSelStatMultiPlotMultiPop <- function(drift.list, stab.list, StatMap, y.axis, n.traits = 10){
    data.drift <- laply(drift.list, function (x) StatMap(x$p.cov))
    data.stab <- laply(stab.list, function (x) StatMap(x$p.cov))
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

TimeSeriesMantel <- function(cor.list, num.cores = 4){
    if (num.cores > 1) {
        library(doMC)
        library(foreach)
        registerDoMC(num.cores)
        parallel = TRUE
    }
    else{
        parallel = FALSE
    }
    n.gen = length(cor.list)
    comparisons <- aaply(1:(n.gen-1), 1, function(x) MantelCor(cor.list[[x]], cor.list[[x+1]], iterations = 1)[1])
    return(comparisons)
}
