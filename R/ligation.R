ligation <- function(molar.ratio, vector.mass, vector.size, insert.size)
{
  insert.mass <- vector.mass*molar.ratio*(insert.size/vector.size)
  return(insert.mass)
}