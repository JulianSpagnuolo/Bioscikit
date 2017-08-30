moles <- function(mol.weight=NULL, mass=NULL, moles=NULL)
{
  if(is.null(mol.weight))
  {
    out <- moles*mass
  }
  if(is.null(mass))
  {
    out <- mol.weight/moles
  }
  if(is.null(moles))
  {
    out <- mass/mol.weight
  }
  return(out)
}