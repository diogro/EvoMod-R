\name{AVGRatioPlot}
\alias{AVGRatioPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
AVGRatioPlot(pop.list, modules = FALSE, num.cores = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pop.list}{
%%     ~~Describe \code{pop.list} here~~
}
  \item{modules}{
%%     ~~Describe \code{modules} here~~
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
function (pop.list, modules = FALSE, num.cores = 2) 
{
    avg <- ldply(pop.list, function(x) AVGRatio(x$p.cor, x$selection.strength, 
        num.cores = num.cores), .progress = "text")
    names(avg)[6] = "Avg_Ratio"
    if (modules) {
        m.avg = melt(avg[, -c(2, 3, 6)], id.vars = c(".id", "Selection_Strength"))
        m.avg = m.avg[!((m.avg[".id"] != "Full Integration") & 
            (m.avg["variable"] == "AVG-")), ]
        m.avg = m.avg[!((m.avg[".id"] == "Full Integration") & 
            (m.avg["variable"] == "AVG+")), ]
        avg.plot = ggplot(m.avg, aes(Selection_Strength, value, 
            group = interaction(variable, Selection_Strength, 
                .id), colour = interaction(.id, variable))) + 
            layer(geom = "boxplot") + labs(x = "Selection Strength", 
            y = "Average Correlation", color = "Module") + scale_colour_discrete(labels = c("Within Module 1", 
            "Within Module 2", "Between Modules")) + theme_bw()
    }
    else {
        avg.full = avg[avg[".id"] == "Full Integration", -3]
        avg.plot = ggplot(avg.full, aes(Selection_Strength, Avg_Ratio, 
            group = Selection_Strength)) + layer(geom = "boxplot") + 
            labs(x = "Selection Strength", y = "AVG Ratio") + 
            theme_bw()
    }
    return(avg.plot)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
