\name{CalcMeanStat}
\alias{CalcMeanStat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CalcMeanStat(mat.list, Stat, nsk = 1000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat.list}{
%%     ~~Describe \code{mat.list} here~~
}
  \item{Stat}{
%%     ~~Describe \code{Stat} here~~
}
  \item{nsk}{
%%     ~~Describe \code{nsk} here~~
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
function (mat.list, Stat, nsk = 1000) 
{
    n.traits = dim(mat.list[[1]])[1]
    beta.mat <- array(rnorm(n.traits * nsk), c(n.traits, nsk))
    beta.mat <- apply(beta.mat, 2, Normalize)
    out = lapply(mat.list, function(mat) {
        return(mean(apply(beta.mat, 2, Stat, cov.matrix = mat)))
    })
    return(unlist(out))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
