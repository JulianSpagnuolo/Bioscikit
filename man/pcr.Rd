\name{pcr}
\alias{pcr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
PCR Setup Calculator
}
\description{
Calculates the volumes of stock PCR reagents required for a single template
}
\usage{
pcr(stock.fwd.prim, stock.rev.prim, stock.dNTP, stock.PCR.buffer, stock.DNApol, final.fwd.prim = 0.2, final.rev.prim = 0.2, final.dNTP, final.PCR.buffer = 1, final.DNApol, stock.template, final.template, rxn.vol = 50)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stock.fwd.prim}{
Stock concentration of forward primer, usually in micro moles per Litre
}
  \item{stock.rev.prim}{
Stock concentration of reverse primer, usually in micro moles per Litre
}
  \item{stock.dNTP}{
Stock concentration of dNTP mix, can be in micromoles per litre or an arbitrary concentration like 100x
}
  \item{stock.PCR.buffer}{
Stock concentration of PCR buffer
}
  \item{stock.DNApol}{
Stock concentration of DNA polymerase, can be in units per microLitre or an arbitrary concentration like 100x
}
  \item{stock.template}{
Initial concentration of template DNA, usually in ng per microlitre
}
  \item{final.fwd.prim}{
Desired concentration of forward primer in final PCR volume, defualt is 200 nM
}
  \item{final.rev.prim}{
Desired concentration of reverse primer in final PCR volume, defualt is 200 nM
}
  \item{final.dNTP}{
Desired concentration of dNTPs in the final PCR volume
}
  \item{final.PCR.buffer}{
Desired concentration of PCR Buffer in the final PCR volume, usually 1x
}
  \item{final.DNApol}{
Desired final concentration of DNA polymerase in the PCR reaction
}
  \item{final.template}{
Desired final concentration of template DNA in the PCR reaction
}
  \item{rxn.vol}{
Desired total volume of the PCR reaction, usually 50 microlitres
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a data.frame of values corresponding to the volumes of stock solutions required.
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
pcr(stock.fwd.prim = 6.25, stock.rev.prim = 6.25, stock.dNTP = 10, stock.PCR.buffer = 10, stock.DNApol = 50, stock.template = 20, final.fwd.prim = 0.2, final.rev.prim =0.2 , final.dNTP = 1, final.PCR.buffer = 1, final.DNApol = 1, final.template = 0.1, rxn.vol = 50)
}

\keyword{ ~pcr }
\keyword{ ~pc }
