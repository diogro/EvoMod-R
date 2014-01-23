\name{PlotPop}
\alias{PlotPop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotPop(pop.path, n.traits)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pop.path}{
%%     ~~Describe \code{pop.path} here~~
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
function (pop.path, n.traits) 
{
    mean.phenotype.plot <- time.series.plot(paste(pop.path, "phenotype.dat", 
        sep = "/"), "mean phenotype", n.traits, T)
    g.var.plot <- time.series.plot(paste(pop.path, "g.var.dat", 
        sep = "/"), "genetic variance", n.traits, F)
    p.var.plot <- time.series.plot(paste(pop.path, "p.var.dat", 
        sep = "/"), "phenotypic variance", n.traits, F)
    h.var.plot <- time.series.plot(paste(pop.path, "h.var.dat", 
        sep = "/"), "heritability", n.traits, F)
    g.corr.plot <- time.series.plot(paste(pop.path, "g.corr.dat", 
        sep = "/"), "genetic correlations", n.traits, T, T)
    g.avg.corr.plot <- AVGCorrPlot(paste(pop.path, "g.corr.dat", 
        sep = "/"), "mean genetic correlations", n.traits)
    p.corr.plot <- time.series.plot(paste(pop.path, "p.corr.dat", 
        sep = "/"), "phenotypic correlations", n.traits, T, T)
    p.avg.corr.plot <- AVGCorrPlot(paste(pop.path, "p.corr.dat", 
        sep = "/"), "mean phenotypic correlations", n.traits)
    plots = list(g.var = g.var.plot, p.var = p.var.plot, h.var = h.var.plot, 
        mean.phenotype = mean.phenotype.plot, g.corr = g.corr.plot, 
        p.corr = p.corr.plot, g.avg.corr = g.avg.corr.plot, p.avg.corr = p.avg.corr.plot)
    return(plots)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
