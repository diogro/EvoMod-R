\name{WithInMultiPlot}
\alias{WithInMultiPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
WithInMultiPlot(file.name, pattern = "DivSel*", n.traits)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file.name}{
%%     ~~Describe \code{file.name} here~~
}
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
function (file.name, pattern = "DivSel*", n.traits) 
{
    require(ggplot2)
    y.axis = "Mean Within Module Correlation"
    folders <- dir("output/", pattern)
    aux.file = paste("output", folders[1], file.name, sep = "/")
    data.phenotype = SetDataFrame(aux.file, n.traits, T)
    n.corrs = n.traits
    n.corrs = (n.corrs * n.corrs - n.corrs)
    generation.vector = rep(seq(data.phenotype$generation[1], 
        data.phenotype$generation[length(data.phenotype$generation)]), 
        each = n.corrs)
    n.gen = length(generation.vector)
    n.pop = length(folders)
    data.avg = array(dim = c(n.gen * n.pop, 4))
    for (pop in 1:(length(folders))) {
        aux.file = paste("output", folders[pop], file.name, sep = "/")
        ploted.data = SetDataFrame(aux.file, n.traits, T)
        module.label = as.character(ploted.data$module)
        module.label[module.label != "between module"] = "within module"
        ploted.data = ploted.data[, 1]
        lower = 1 + ((pop - 1) * n.gen)
        upper = pop * n.gen
        print(folders[pop])
        aux.file.name = "pop.parameters.txt"
        aux.file = paste("output", folders[pop], aux.file.name, 
            sep = "/")
        parameters = scan(aux.file, character())
        index = which("theta" == parameters) + 2
        selection.label = rep(as.numeric(parameters[index]), 
            n.gen)
        data.avg[lower:upper, 1] = generation.vector
        data.avg[lower:upper, 2] = ploted.data
        data.avg[lower:upper, 3] = selection.label
        data.avg[lower:upper, 4] = module.label
    }
    data.avg = data.frame(as.numeric(data.avg[, 1]), as.numeric(data.avg[, 
        2]), data.avg[, 3], data.avg[, 4])
    names(data.avg) = c("generation", "ploted.data", "Selection_Strengh", 
        "Correlation_Type")
    data.avg = aggregate(data.avg$ploted.data, list(data.avg$generation, 
        data.avg$Correlation_Type, data.avg$Selection_Strengh), 
        mean)
    names(data.avg) = c("generation", "Correlation_Type", "Selection_Strengh", 
        "ploted.data")
    data.avg = data.frame(data.avg, module_sel = paste(as.character(data.avg[, 
        2]), as.character(data.avg[, 3])))
    time.series <- ggplot(data.avg, aes(generation, ploted.data, 
        group = module_sel, color = Selection_Strengh)) + layer(geom = "smooth") + 
        scale_y_continuous(y.axis)
    return(time.series)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
