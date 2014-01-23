\name{NoSelStatMultiPlotMultiPop}
\alias{NoSelStatMultiPlotMultiPop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
NoSelStatMultiPlotMultiPop(drift.list, stab.list, StatMap, y.axis, n.traits = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{drift.list}{
%%     ~~Describe \code{drift.list} here~~
}
  \item{stab.list}{
%%     ~~Describe \code{stab.list} here~~
}
  \item{StatMap}{
%%     ~~Describe \code{StatMap} here~~
}
  \item{y.axis}{
%%     ~~Describe \code{y.axis} here~~
}
  \item{n.traits}{
%%     ~~Describe \code{n.traits} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (drift.list, stab.list, StatMap, y.axis, n.traits = 10) 
{
    data.drift <- laply(drift.list, function(x) StatMap(x$p.cov))
    data.stab <- laply(stab.list, function(x) StatMap(x$p.cov))
    data.drift <- adply(data.drift, 2, function(x) c(mean(x), 
        quantile(x, 0.025), quantile(x, 0.975)))
    data.stab <- adply(data.stab, 2, function(x) c(mean(x), quantile(x, 
        0.025), quantile(x, 0.975)))
    data.drift[, 5] = rep("Drift", length(data.drift))
    data.stab[, 5] = rep("Stabilizing", length(data.stab))
    data.avg = data.frame(rbind(data.drift, data.stab))
    data.avg[, 1] = as.numeric(levels(data.avg[, 1]))[data.avg[, 
        1]]
    names(data.avg) = c("generation", "stat_mean", "stat_lower", 
        "stat_upper", "Selection_scheme")
    time.series <- ggplot(data.avg, aes(generation, stat_mean, 
        color = Selection_scheme)) + geom_smooth(aes(ymin = stat_lower, 
        ymax = stat_upper, color = Selection_scheme), data = data.avg, 
        stat = "identity") + scale_y_continuous(y.axis) + scale_x_continuous("Generation")
    return(time.series)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
