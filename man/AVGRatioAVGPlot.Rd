\name{AVGRatioAVGPlot}
\alias{AVGRatioAVGPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
AVGRatioAVGPlot(file.name, pattern = "Drift*", n.traits)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file.name}{
%%     ~~Describe \code{file.name} here~~
}
  \item{pattern}{
%%     ~~Describe \code{pattern} here~~
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
function (file.name, pattern = "Drift*", n.traits) 
{
    require(ggplot2)
    y.axis = "AVG Ratio"
    folders <- dir("output/", pattern)
    aux.file = paste("output", folders[1], file.name, sep = "/")
    data.corr = SetDataFrame(aux.file, n.traits, T)
    generation.vector = seq(data.corr$generation[1], data.corr$generation[length(data.corr$generation)])
    n.gen = length(generation.vector)
    n.pop = length(folders)
    data.avg = array(dim = c(n.gen * n.pop, 2))
    for (pop in 1:(length(folders))) {
        aux.file = paste("output", folders[pop], file.name, sep = "/")
        AVGRatio <- AVGRatioCalc(aux.file, n.traits)
        lower = 1 + ((pop - 1) * n.gen)
        upper = pop * n.gen
        print(lower)
        print(upper)
        data.avg[lower:upper, 1] = generation.vector
        data.avg[lower:upper, 2] = AVGRatio
    }
    data.avg = as.data.frame(data.avg)
    names(data.avg) = c("generation", "AVGRatio")
    time.series <- ggplot(data.avg, aes(generation, AVGRatio)) + 
        scale_y_continuous(y.axis) + geom_point(alpha = 1/500) + 
        geom_smooth() + stat_smooth(geom = "ribbon")
    return(time.series)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
