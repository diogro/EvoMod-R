\name{AVGCorrPlot}
\alias{AVGCorrPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
AVGCorrPlot(input.file, y.axis, n.traits)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input.file}{
%%     ~~Describe \code{input.file} here~~
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
function (input.file, y.axis, n.traits) 
{
    require(ggplot2)
    data.corr = SetDataFrame(input.file, n.traits, T)
    data.avg = data.frame()
    module.name = unique(data.corr$module)
    for (i in 1:length(module.name)) {
        aux.data = data.corr[data.corr$module == module.name[i], 
            ]
        aux.data = as.vector(tapply(aux.data$main, aux.data$generation, 
            mean, module = module.name[i]))
        data.avg = rbind(data.avg, data.frame(generation = seq(data.corr$generation[1], 
            data.corr$generation[length(data.corr$generation)]), 
            main = aux.data, module = module.name[i]))
    }
    time.series <- ggplot(data.avg, aes(generation, main, group = module, 
        color = module)) + layer(geom = "line") + scale_y_continuous(y.axis)
    return(time.series)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
