
d_shadedErrorRegion <- function(XVect, YVect, YError, colorVals, lineType, plotNewFlag) {
  
  #XVect <- seq(0,8000,1) # i.e., it ranges from 0 to 8000 in steps of 1
  #YVect <- dnorm(((XVect/1000)-4),0,1) + 0.4
  #YError <- rep(0.05, times = length(YVect)) # upper range for cond A
  #colorVals = rgb(0.7, 0.3, 0,0.5)
  
  
  #XVect <- FixToSP1End_SO$Time
  #YVect <- FixToSP1End_SO$Sp1_Sum
  #YError <- FixToSP1End_SO$Sp1_Sum_SE
  #colorVals = colorVals
  
  YError[is.na(YError)] = 0
  YVect[is.na(YVect)] = 0
  
  # First make an empty plot (with the type = "n" command))
  if (plotNewFlag == 1) {
  plot(XVect,YVect, type = "n",xlab="",ylab="")
  }
  
  #Then plot the error regions:
  polygon(c(XVect,rev(XVect)),c(YVect+YError,rev(YVect-YError)), border = NA, col=colorVals)
  # then plot the mean value on top:
  lines(XVect,YVect,lty= lineType)

}

