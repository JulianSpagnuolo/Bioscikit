\name{dsDNAends}
\alias{dsDNAends}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Linear DNA: mass to moles of ends calculator
}
\description{
Determines the number of moles of ends of a given mass and size of linear dsDNA
}
\usage{
dsDNAends(mass = NULL, size = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mass}{
mass of linear dsDNA
}
  \item{size}{
size of linear dsDNA in base pairs
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
returns the number of moles of linear dsDNA ends
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Julian Spagnuolo
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
function (mass = NULL, size = NULL)
{
    if (is.null(mass) | is.null(size)) {
        cat("Missing required parameters!!\n")
    }
    else {
        out <- mass/((size * 617.96) + 36.04) * 2
        return(out)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~dsDNA }
\keyword{ ~ends }
