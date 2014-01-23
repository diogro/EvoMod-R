\name{ReadVariances}
\alias{ReadVariances}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ReadVariances(input.file, n.traits)
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
    data.init = read.table(input.file)
    gen.number = data.init[seq(1, length(data.init[, 1]), n.traits + 
        1), ]
    raw.trait.means = data.init[-seq(1, length(data.init[, 1]), 
        n.traits + 1), ]
    generations = length(gen.number)
    var.vectors = vector("list", generations)
    for (gen in 1:generations) {
        lower = 1 + ((gen - 1) * n.traits)
        upper = gen * n.traits
        raw.trait.means[lower:upper]
        var.vectors[[gen]] = raw.trait.means[lower:upper]
    }
    names(var.vectors) = gen.number
    return(var.vectors)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
