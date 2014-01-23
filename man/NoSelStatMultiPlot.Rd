\name{NoSelStatMultiPlot}
\alias{NoSelStatMultiPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
NoSelStatMultiPlot(pop.list, StatMap, y.axis, n.traits = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pop.list}{
%%     ~~Describe \code{pop.list} here~~
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
function (pop.list, StatMap, y.axis, n.traits = 10) 
{
    data.avg <- laply(pop.list, function(x) StatMap(x$p.cov))
    data.avg <- adply(data.avg, 2, function(x) c(mean(x), quantile(x, 
        0.025), quantile(x, 0.975)))
    data.avg[, 1] = as.numeric(levels(data.avg[, 1]))[data.avg[, 
        1]]
    names(data.avg) = c("generation", "stat_mean", "stat_lower", 
        "stat_upper")
    time.series <- ggplot(data.avg, aes(generation, stat_mean)) + 
        geom_smooth(aes(ymin = stat_lower, ymax = stat_upper), 
            data = data.avg, stat = "identity") + scale_y_continuous(y.axis) + 
        scale_x_continuous("Generation")
    return(time.series)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
