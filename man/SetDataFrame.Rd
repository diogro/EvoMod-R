\name{SetDataFrame}
\alias{SetDataFrame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SetDataFrame(input.file, n.traits, corr.plot)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input.file}{
%%     ~~Describe \code{input.file} here~~
}
  \item{n.traits}{
%%     ~~Describe \code{n.traits} here~~
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
function (input.file, n.traits, corr.plot) 
{
    data.init = read.table(input.file)
    if (corr.plot) {
        aux.trait = rep(c(1, -2), each = n.traits/2)
        aux.trait = aux.trait \%*\% t(aux.trait)
        aux.trait = aux.trait[upper.tri(aux.trait)]
        aux.selection = aux.trait
        aux.trait[aux.trait == 1] = "within module 1"
        aux.trait[aux.trait == 4] = "within module 2"
        aux.trait[aux.trait == -2] = "between module"
        aux.selection[aux.selection == 1] = "Positive"
        aux.selection[aux.selection == 4] = "Negative"
        aux.selection[aux.selection == -2] = "Divergent"
        n.traits = (n.traits * n.traits - n.traits)/2
    }
    else {
        aux.trait = rep(c(1, -2), each = n.traits/2)
        aux.trait[aux.trait == 1] = "module 1"
        aux.trait[aux.trait == -2] = "module 2"
    }
    gen.number = data.init[seq(1, length(data.init[, 1]), n.traits + 
        1), ]
    raw.trait.means = data.init[-seq(1, length(data.init[, 1]), 
        n.traits + 1), ]
    generations = length(gen.number)
    gen.number = rep(gen.number, each = n.traits)
    if (corr.plot) {
        sel.scheme = rep(aux.selection, generations)
    }
    else {
        sel.scheme = rep(rep(c("positive", "negative"), each = n.traits/2), 
            generations)
    }
    module = as.character(rep(aux.trait, generations))
    trait.name = as.factor(rep(1:n.traits, generations))
    data.clean = data.frame(raw.trait.means, trait.name, gen.number, 
        sel.scheme, module)
    names(data.clean) = c("main", "trait", "generation", "selection", 
        "module")
    return(data.clean)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
