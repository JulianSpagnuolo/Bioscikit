\name{conc}
\alias{conc}

\title{
concentration
}
\description{
Calculates one of either volume, moles or molar concentration of a solution.
}
\usage{
conc(volume = NULL, moles = NULL, molarity = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{volume}{
Desired volume of solution in Litres, if set to NULL function will return the volume of solution needed to meet the other parameters
}
  \item{moles}{
Desired moles in solution, if set to NULL function will return the moles needed to meet the other parameters
}
  \item{molarity}{
Desired molar concentration of solution in moles per Litre, if set to NULL function will return the molar concentration of solution needed to meet the other parameters
}
}
\details{

}
\value{
returns a numeric value coresponding to the missing parameter, one of either moles, volume or molarity
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
### Returns required volume:
conc(moles=1, molarity=0.5)

### Returns required moles:
conc(volume=0.5, molarity=1)

### Returns molar concentration:
conc(volume=2, moles=0.5)
}

\keyword{ ~conc }
\keyword{ ~cnc }
