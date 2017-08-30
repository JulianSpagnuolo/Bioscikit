dsDNAends <- function(mass=NULL, size=NULL)
{
  if(is.null(mass) | is.null(size))
  {
    cat("Missing required parameters!!\n")
  }
  else
  {
      out <- mass/((size*617.96)+36.04) * 2
      return(out)
  }
}
