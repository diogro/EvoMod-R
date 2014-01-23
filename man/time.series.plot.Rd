\name{time.series.plot}
\alias{time.series.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
time.series.plot(input.file, y.axis, n.traits, selection = T, corr.plot = F)
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
  \item{selection}{
%%     ~~Describe \code{selection} here~~
}
  \item{corr.plot}{
%%     ~~Describe \code{corr.plot} here~~
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
function (input.file, y.axis, n.traits, selection = T, corr.plot = F) 
{
    require(ggplot2)
    data.clean = SetDataFrame(input.file, n.traits, corr.plot)
    if (selection) 
        time.series <- ggplot(data.clean, aes(generation, main, 
            group = trait, color = selection)) + layer(geom = "point") + 
            scale_y_continuous(y.axis)
    else if (corr.plot) 
        time.series <- ggplot(data.clean, aes(generation, main, 
            group = trait, color = module)) + layer(geom = c("point", 
            "line")) + scale_y_continuous(y.axis)
    else time.series <- ggplot(data.clean, aes(generation, main, 
        group = trait, color = trait)) + layer(geom = "point") + 
        scale_y_continuous(y.axis)
    return(time.series)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
