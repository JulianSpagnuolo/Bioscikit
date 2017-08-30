\name{moles}
\alias{moles}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Molecular Weight Calculator
}
\description{
calculates the molecular weight, mass or number of moles of a reagent
}
\usage{
moles(mol.weight = NULL, mass = NULL, moles = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mol.weight}{
Molecular weight of reagent in moles per gram
}
  \item{mass}{
mass of reagent in grams
}
  \item{moles}{
moles of reagent
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
## Calulate the mass of Tris-Base required for 2 moles
moles(mol.weight = 121.14, moles = 2)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~mol }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~mole }% __ONLY ONE__ keyword per line
