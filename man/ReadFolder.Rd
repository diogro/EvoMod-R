\name{ReadFolder}
\alias{ReadFolder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ReadFolder(input.folder, n.traits = 10, sel.type, direct.sel = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input.folder}{
%%     ~~Describe \code{input.folder} here~~
}
  \item{n.traits}{
%%     ~~Describe \code{n.traits} here~~
}
  \item{sel.type}{
%%     ~~Describe \code{sel.type} here~~
}
  \item{direct.sel}{
%%     ~~Describe \code{direct.sel} here~~
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
function (input.folder, n.traits = 10, sel.type, direct.sel = T) 
{
    print(input.folder)
    input.folder = paste("./output", input.folder, sep = "/")
    input.file = paste(input.folder, "p.corr.dat", sep = "/")
    p.cor = ReadMatrices(input.file, n.traits)
    input.file = paste(input.folder, "g.corr.dat", sep = "/")
    g.cor = ReadMatrices(input.file, n.traits)
    input.file = paste(input.folder, "p.var.dat", sep = "/")
    p.var = ReadVariances(input.file, n.traits)
    input.file = paste(input.folder, "g.var.dat", sep = "/")
    g.var = ReadVariances(input.file, n.traits)
    input.file = paste(input.folder, "h.var.dat", sep = "/")
    h.var = ReadVariances(input.file, n.traits)
    input.file = paste(input.folder, "phenotype.dat", sep = "/")
    phenotype = ReadVariances(input.file, n.traits)
    p.cov = CalcCovar(p.cor, p.var)
    g.cov = CalcCovar(g.cor, g.var)
    if (direct.sel) {
        aux.file = paste(input.folder, "pop.parameters.txt", 
            sep = "/")
        parameters = scan(aux.file, character(), quiet = TRUE)
        index = which("theta" == parameters) + 2
        selection.strength = as.numeric(parameters[index])
    }
    else {
        selection.strength = 0
    }
    out.list = list(p.cor = p.cor, g.cor = g.cor, p.var = p.var, 
        g.var = g.var, h.var = h.var, p.cov = p.cov, g.cov = g.cov, 
        selection.type = sel.type, selection.strength = selection.strength, 
        generation = as.numeric(names(p.var)))
    return(out.list)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
