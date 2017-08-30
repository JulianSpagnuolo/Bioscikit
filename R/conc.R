conc <- function(volume=NULL, moles=NULL, molarity=NULL)
{
  if(is.null(volume))
  {
    out <- moles/molarity
  }
  if(is.null(moles))
  {
    out <- molarity*volume
  }
  if(is.null(molarity))
  {
    out <- moles/volume
  }
  return(out)
}
