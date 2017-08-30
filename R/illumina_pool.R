illumina.pool <- function(stock.conc, df=NULL, read.share=NULL, pool.conc=10, pool.vol=20, print=TRUE)
#' @author Julian Spagnuolo
#' @title Pooling Illumina Libraries
#' @param stock.conc numeric vector. Initial (stock) concentrations of inidivual libraries to be pooled in nM.
#' @param df numeric vector. Dilution factor of inidividual libraries to be pooled (if they should be diluted before poolling). If NULL then it uses the stock concentration. Length must equal that of stock.conc. Default is NULL.
#' @param read.share numeric vector. Share of reads that each independent library will take in the total pool. Length must equal that of stock.conc, if NULL will default to 1 (i.e. equal shares). Default is NULL.
#' @param pool.conc numeric. Final overall concentration of the pooled library in nM. Default is 10 nM.
#' @param pool.vol numeric. Final desired volume of the pooled library in uL. Default is 20 uL.
#' @param print logical. Whether to print results to console, does not affect printing of diluent volume or other summary stats. Default is TRUE.
{
  if(is.null(df))
  {
    df <- rep(1, length(stock.conc))
  }
  if(length(df) != length(stock.conc))
  {
    stop("Number of stock.concs or dilutions does not match!!\n")
  }

  if(is.null(read.share))
  {
    read.share <- rep(1, length(stock.conc))
  }
  if(length(read.share) != length(stock.conc))
  {
    stop("Number of stock.concs or read.shares does not match!!\n")
  }

  results <- data.frame(stock.conc=vector(length=length(stock.conc)),
                        read.share=vector(length=length(stock.conc)),
                        df=vector(length=length(stock.conc)),
                        vol.to.add=vector(length=length(stock.conc)),
                        conc.in.pool=vector(length=length(stock.conc)))

  results$stock.conc <- stock.conc
  results$read.share <- read.share
  results$df <- df
  results$conc.in.pool <- (pool.conc*read.share)/sum(read.share)
  results$vol.to.add <- (results$conc.in.pool*pool.vol)/(stock.conc/df)

  if(sum(results$vol.to.add) > pool.vol)
  {
    warning("Total volume of pooled libraries exceeeds desired total pool volume, adjust parameters!!\n", immediate. = TRUE)
  }

  cat("Total volume of libraries to pool: ", sum(results$vol.to.add)," uL (microlitres)\n")
  cat("Volume of diluent to add: ", pool.vol-sum(results$vol.to.add)," uL (microlitres)\n")

  if(print == TRUE)
  {
    head(results, n = nrow(results))
  }
  return(results)
}
