\name{ssDNAmm}
\alias{ssDNAmm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ssDNA moles-to-mass conversion
}
\description{
Converts between mass, in grams, of ssDNA and moles of ssDNA of a given size in nucleotides.
}
\usage{
ssDNAmm(mass = NULL, size = NULL, moles = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mass}{
mass of ssDNA in grams
}
  \item{size}{
size of ssDNA in nucleotides
}
  \item{moles}{
number of moles of ssDNA
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
ssDNAmm(mass = 10, size = 2000)

## Convert from moles to mass
ssDNAmm(moles = 0.005, size = 2400)

}

\keyword{ ~ssDNA }
\keyword{ ~ssdna }
