\name{dilution}
\alias{dilution}

\title{
Dilution
}
\description{
Calculates one of either stock concentration, final concentration, require volume of stock solution or final volume of diluted solution depending on given parameters.
}
\usage{
dilution(c1 = NULL, v1 = NULL, c2 = NULL, v2 = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{c1}{
concentration of stock solution
}
  \item{v1}{
volume of stock solution
}
  \item{c2}{
concentration of diluted solution
}
  \item{v2}{
volume of diluted solution
}
}
\details{
Make sure that the units of concentration and volume are the same. i.e if using mL for v1, use mL for v2, if using ng/mL for c1 use ng/mL for c2, etc.
}
\value{
returns the value of the missing parameter in the same units as entered.
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
### For concentration of stock solution:
dilution(v1=100, c2=10, v2=500)

### For concentration of diluted solution:
dilution(c1=20, v1=50, v2=100)

### For volume of stock solution needed to make given volume of solution at a given concentration:
dilution(c1=100, c2=10, v2=100)

### For final volume required to dilute a given volume of stock solution to a desired final concentration:
dilution(c1=100, v1=10, c2=10)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~dil }
\keyword{ ~dilut }
