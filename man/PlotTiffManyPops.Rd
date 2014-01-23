\name{PlotTiffManyPops}
\alias{PlotTiffManyPops}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotTiffManyPops(list.plots, file.name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{list.plots}{
%%     ~~Describe \code{list.plots} here~~
}
  \item{file.name}{
%%     ~~Describe \code{file.name} here~~
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
function (list.plots, file.name) 
{
    require(ggplot2)
    require(gridExtra)
    dir.create("output/images")
    file.names = names(list.plots[[1]])
    for (i in 1:length(file.names)) {
        tiff(paste("output/images/", file.name, ".", file.names[i], 
            ".tiff", sep = ""), width = 1080, height = 1980)
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(3, 2)))
        vplayout <- function(x, y) viewport(layout.pos.row = x, 
            layout.pos.col = y)
        print(list.plots[[1]][[i]], vp = vplayout(1, 1))
        print(list.plots[[2]][[i]], vp = vplayout(1, 2))
        print(list.plots[[3]][[i]], vp = vplayout(2, 1))
        print(list.plots[[4]][[i]], vp = vplayout(2, 2))
        print(list.plots[[5]][[i]], vp = vplayout(3, 1))
        print(list.plots[[6]][[i]], vp = vplayout(3, 2))
        dev.off(dev.cur())
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
