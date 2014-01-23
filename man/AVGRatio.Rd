\name{AVGRatio}
\alias{AVGRatio}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
AVGRatio(mat.list, Selection_Strength, last.gen = T, num.cores = 1, generations = 1:10000 + 20000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat.list}{
%%     ~~Describe \code{mat.list} here~~
}
  \item{Selection_Strength}{
%%     ~~Describe \code{Selection_Strength} here~~
}
  \item{last.gen}{
%%     ~~Describe \code{last.gen} here~~
}
  \item{num.cores}{
%%     ~~Describe \code{num.cores} here~~
}
  \item{generations}{
%%     ~~Describe \code{generations} here~~
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
function (mat.list, Selection_Strength, last.gen = T, num.cores = 1, 
    generations = 1:10000 + 20000) 
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
    n.traits = dim(mat.list[[1]])[1]
    module.1 = rep(c(1, 0), each = n.traits/2)
    module.2 = rep(c(0, 1), each = n.traits/2)
    modularity.hipot = cbind(module.1, module.2)
    if (last.gen) 
        AVGRatio <- ldply(mat.list[length(mat.list)], function(x) TestModularity(x, 
            modularity.hipot, iterations = 0), .parallel = parallel)
    else {
        AVGRatio <- ldply(mat.list, function(x) TestModularity(x, 
            modularity.hipot, iterations = 0), .parallel = parallel)
        AVGRatio["generation"] = rep(generations, each = 3)
    }
    AVGRatio["Selection_Strength"] = Selection_Strength
    return(AVGRatio)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
