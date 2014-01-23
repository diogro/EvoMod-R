\name{AVGRatioSinglePlot}
\alias{AVGRatioSinglePlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
AVGRatioSinglePlot(input.file, n.traits)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input.file}{
%%     ~~Describe \code{input.file} here~~
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
function (input.file, n.traits) 
{
    require(ggplot2)
    y.axis = "AVGRatio"
    data.corr = SetDataFrame(input.file, n.traits, T)
    AVGRatio <- AVGRatioCalc(input.file, n.traits)
    data.avg = data.frame(generation = seq(data.corr$generation[1], 
        data.corr$generation[1] + length(AVGRatio) - 1), main = AVGRatio)
    time.series <- ggplot(data.avg, aes(generation, main)) + 
        layer(geom = "line") + scale_y_continuous(y.axis)
    return(time.series)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
