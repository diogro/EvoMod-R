\name{PlotPngSinglePop}
\alias{PlotPngSinglePop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotPngSinglePop(list.plots, file.name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{list.plots}{
%%     ~~Describe \code{list.plots} here~~
}
  \item{file.name}{
%%     ~~Describe \code{file.name} here~~
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
function (list.plots, file.name) 
{
    require(ggplot2)
    require(gridExtra)
    dir.create("output/images")
    file.names = names(list.plots)
    for (i in 1:length(file.names)) {
        png(paste("output/images/", file.name, ".", file.names[i], 
            ".png", sep = ""), width = 700, height = 300)
        print(list.plots[[i]])
        dev.off(dev.cur())
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
