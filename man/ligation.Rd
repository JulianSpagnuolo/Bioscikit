\name{ligation}
\alias{ligation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Ligation Calculator
}
\description{
Calculates the mass of insert fragment required for a ligation reaction.
}
\usage{
ligation(molar.ratio, vector.mass, vector.size, insert.size)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{molar.ratio}{
Molar ratio of insert to vector
}
  \item{vector.mass}{
mass of vector fragment used in ligation reaction, usually in ng
}
  \item{vector.size}{
size of vector fragment to ligate in kilo base pairs
}
  \item{insert.size}{
size of insert fragment to ligate in kilo base pairs
}
}
\details{
returns the mass of the insert fragment in the same units as the vector mass, usually ng.
}
\value{
numeric value corresponding to the mass of the insert fragment in the same units as the vector mass, usually ng.
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
## Calculating a ligation of a 1 kBP insert to 180 ng of a 2.7 kBP vector using insert-to-vector ratio of 2:1

ligation(molar.ratio = 2/1, vector.mass = 180, vector.size = 2.7, insert.size = 1)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~lig }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~ligat }% __ONLY ONE__ keyword per line
