growth <- function(data=NULL, OD=NULL, time.points=NULL, condition=NULL, plot=FALSE, wavelength=600, time.units="min", verbose=TRUE)
{
  if(is.null(data))
  {
    if(is.null(condition))
    {
      condition <- rep(1, length(time.points))
    }
    data <- data.frame(time.points=time.points, OD=OD, condition=condition)
    model <- vector(mode = "list", length = length(unique(data$condition)))
    for(i in as.factor(data$condition))
    {
      ### Switch this to logistic growth model
      model[[i]] <- lm(formula=log10(OD)~time.points, data=data[which(data$condition == i),])

      if(verbose == TRUE)
      {
        cat("-----------------\n","Growth Rate Model for condition ", i,"\n","-----------------\n")
        print(summary(model[[i]]))
        cat("-----------------\n")
      }
    }
  }
  if(is.null(OD) & is.null(time.points))
  {
    if(is.null(data$condition))
    {
      data$condition <- rep(1, length(data$OD))
    }
    model <- vector(mode = "list", length = length(unique(data$condition)))
    names(model) <- unique(data$condition)
    for(i in as.factor(data$condition))
    {
      model[[i]] <- lm(formula=log10(OD)~time.points, data=data[which(data$condition == i),])
      if(verbose == TRUE)
      {
        cat("-----------------\n","Growth Rate Model for condition ", i,"\n","-----------------\n")
        print(summary(model[[i]]))
        cat("-----------------\n")
      }
    }
  }
  if(plot == TRUE)
  {
    plot <- ggplot() +geom_point(data=data, aes(x=time.points, y=OD, colour=as.factor(data$condition))) +geom_smooth(data=data,aes(x=time.points, y=OD, colour=as.factor(data$condition)), method="lm") +scale_y_log10() +theme_bw() +theme(legend.position="right") +labs(x=paste("Time (", time.units,")", sep=""), y=paste("Optical Density (",wavelength," nm)", sep=""))
    print(plot)
    return(model)
  }
  if(plot == FALSE)
  {
    return(model)
  }
}
