\name{dsDNAmm}
\alias{dsDNAmm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
dsDNA moles-to-mass conversion
}
\description{
Converts between mass, in grams, of dsDNA and moles of dsDNA of a given size in base pairs.
}
\usage{
dsDNAmm(mass = NULL, size = NULL, moles = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mass}{
mass of dsDNA used in grams
}
  \item{size}{
size of dsDNA in base pairs
}
  \item{moles}{
number of moles of dsDNA
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
## Convert from mass to moles
dsDNAmm(mass = 10, size = 2000)

## Convert from moles to mass
dsDNAmm(moles = 0.005, size = 2400)

}

\keyword{ ~dsDNA }
\keyword{ ~dsdna }
