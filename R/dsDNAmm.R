dsDNAmm <- function(mass=NULL, size=NULL, moles=NULL)
{
  if(is.null(moles))
  {
    out <- mass/((size*617.96)+36.04)
    return(out)
  }
  if(is.null(mass))
  {
    out <- moles*((size*617.96)+36.04)
    return(out)
  }
  if(is.null(size))
  {
    cat("Missing size")
  }
}
