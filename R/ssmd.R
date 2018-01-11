ssmd <- function(data, samples, controls)
{
  #' @title Strictly Standardised Mean Difference
  #' @author Julian Spagnuolo
  #' @description Calculates the strictly standardised mean difference between normalised control and sample (test) count data. Based on Zhang, XD. et al Journal of Biomolecular Screening 12(4); 2007, (DOI:10.1177/1087057107300646).
  #' @param data data.frame or matrix containing normalised count data for control and test replicates.
  #' @param samples vector of column names corresponding to test replicates in data
  #' @param controls vector of column names corresponding to control replicates in data

  ssmd <- vector(length=nrow(data))
  mu.control <- base::rowMeans(controls)
  mad.control <- apply(X = as.matrix(controls), MARGIN=1, FUN = stats::mad)
  mad.control <- mad.control^2
  n.control <- ncol(controls)
  n.samp <- ncol(data[,samples])
  k.factor <- 2/(n.samp+n.control - 3.5)

  for(i in 1:nrow(data))
  {
    mu.diff <- (rowMeans(data[i,samples]) - mu.control[i])
    mad.samp <- stats::mad(as.matrix(data[i,samples]))^2
    ssmd[i] <- mu.diff/sqrt(k.factor*((n.samp - 1)*mad.samp + (n.control - 1)*mad.control[i]))
  }

  return(ssmd)
}
