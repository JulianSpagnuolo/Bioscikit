dilution <- function(c1=NULL,v1=NULL,c2=NULL,v2=NULL)
{
  if(is.null(c1))
  {
    out <- (c2*v2)/v1
  }
  if(is.null(v1))
  {
    out <- (c2*v2)/c1
  }
  if(is.null(c2))
  {
    out <- (c1*v1)/v2
  }
  if(is.null(v2))
  {
    out <- (c1*v1)/c2
  }
  return(out)
}