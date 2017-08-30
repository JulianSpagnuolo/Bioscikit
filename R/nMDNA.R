nMDNA <- function(conc, size)
{
  nM <- (conc*1e6)/(660*size)
  return(nM)
}
