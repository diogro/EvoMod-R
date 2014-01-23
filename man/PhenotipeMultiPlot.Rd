\name{PhenotipeMultiPlot}
\alias{PhenotipeMultiPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PhenotipeMultiPlot(pattern = "DivSel-Short-1000-*", n.traits)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
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
function (pattern = "DivSel-Short-1000-*", n.traits) 
{
    require(ggplot2)
    file.name = "phenotype.dat"
    y.axis = "Mean phenotype norm"
    folders <- dir("output/", pattern)
    aux.file = paste("output", folders[1], file.name, sep = "/")
    data.phenotype = SetDataFrame(aux.file, n.traits, F)
    generation.vector = rep(seq(data.phenotype$generation[1], 
        data.phenotype$generation[length(data.phenotype$generation)]), 
        each = n.traits)
    n.gen = length(generation.vector)/n.traits
    n.pop = length(folders)
    data.avg = array(dim = c(n.gen * n.pop, 3))
    for (pop in 1:(length(folders))) {
        aux.file = paste("output", folders[pop], file.name, sep = "/")
        ploted.data = abs(as.numeric(SetDataFrame(aux.file, n.traits, 
            F)[, 1]))
        ploted.data = tapply(ploted.data, generation.vector, 
            mean)
        lower = 1 + ((pop - 1) * n.gen)
        upper = pop * n.gen
        print(folders[pop])
        aux.file.name = "pop.parameters.txt"
        aux.file = paste("output", folders[pop], aux.file.name, 
            sep = "/")
        parameters = scan(aux.file, character())
        index = which("theta" == parameters) + 2
        label.vector = rep(as.numeric(parameters[index]), n.gen)
        data.avg[lower:upper, 1] = 1:n.gen + generation.vector[1]
        data.avg[lower:upper, 2] = ploted.data
        data.avg[lower:upper, 3] = label.vector
    }
    data.avg = data.frame(as.numeric(data.avg[, 1]), as.numeric(data.avg[, 
        2]), data.avg[, 3])
    names(data.avg) = c("generation", "ploted.data", "Selection_Strengh")
    time.series <- ggplot(data.avg, aes(generation, ploted.data, 
        group = Selection_Strengh, color = Selection_Strengh)) + 
        layer(geom = "smooth") + scale_y_continuous(y.axis)
    return(time.series)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
