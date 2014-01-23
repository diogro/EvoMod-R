\name{TimeSeriesMantel}
\alias{TimeSeriesMantel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
TimeSeriesMantel(cor.list, num.cores = 4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cor.list}{
%%     ~~Describe \code{cor.list} here~~
}
  \item{num.cores}{
%%     ~~Describe \code{num.cores} here~~
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
function (cor.list, num.cores = 4) 
{
    if (num.cores > 1) {
        library(doMC)
        library(foreach)
        registerDoMC(num.cores)
        parallel = TRUE
    }
    else {
        parallel = FALSE
    }
    n.gen = length(cor.list)
    comparisons <- aaply(1:(n.gen - 1), 1, function(x) MantelCor(cor.list[[x]], 
        cor.list[[x + 1]], iterations = 1)[1])
    return(comparisons)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
