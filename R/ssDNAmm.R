ssDNAmm <- function(mass=NULL, moles=NULL, size=NULL)
{
  if(is.null(moles))
  {
    out <- mass/((size*308.97)+18.02)
    return(out)
  }
  if(is.null(mass))
  {
    out <- moles*((size*308.97)+18.02)
    return(out)
  }
  if(is.null(size))
  {
    cat("Missing size")
  }
}
