\name{StatMultiPlot}
\alias{StatMultiPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
StatMultiPlot(pop.list, StatMap, y.axis, n.traits = 10)
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
    generation.vector = pop.list[[1]]$generation
    n.gen = length(generation.vector)
    n.pop = length(pop.list)
    data.avg = array(dim = c(n.gen * n.pop, 3))
    for (pop in 1:n.pop) {
        stat <- StatMap((pop.list[[pop]]$p.cov))
        print(pop)
        lower = 1 + ((pop - 1) * n.gen)
        upper = pop * n.gen
        label.vector = rep(as.numeric(pop.list[[pop]]$selection.strength), 
            n.gen)
        data.avg[lower:upper, 1] = generation.vector
        data.avg[lower:upper, 2] = stat
        data.avg[lower:upper, 3] = label.vector
    }
    data.avg = data.frame(as.numeric(data.avg[, 1]), as.numeric(data.avg[, 
        2]), data.avg[, 3])
    names(data.avg) = c("generation", "stat", "Selection_Strength")
    time.series <- ggplot(data.avg, aes(generation, stat, group = Selection_Strength, 
        color = Selection_Strength)) + layer(geom = "smooth") + 
        scale_y_continuous(y.axis)
    return(time.series)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
