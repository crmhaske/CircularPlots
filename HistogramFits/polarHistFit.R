## =============================================================================
## Polar histograms with fit line v.1
## By Christie Haskell, 27-07-2014 (v.1)
## Source: https://github.com/crmhaske/CircularPlots/blob/HistogramFits/polarHistFit.R
## =============================================================================

#Function to compute histgrams as rectangles
#Input:
#x - raw data;
#binSize - bin size
#inner Radius - distance from center to start plotting data
#xStart - Data leftmost limit
#xEnd - Data rightermost limit
#Output:
#df - data frame of xmins, xmaxs, ymins and ymaxs for each bin

histxy<-function(x,binSize,innerRadius,xStart,xEnd) {
  x.min<-seq(xStart,xEnd-binSize,binSize)
  x.max<-seq(xStart+binSize,xEnd,binSize)
  y.min<-rep(innerRadius,length(x.min))
  y.max<-hist(x,plot=FALSE,br=seq(xStart,xEnd,(x.max[1]-x.min[1])))$density+y.min
  
  df<-data.frame(x.min,x.max,y.min,y.max)
  return(df)
}

#Plotting function to plot raw data histograms and a fit line
#Required input:
#x - raw data to be fit
#fn - function to fit to
#params - fit paramaters

polarHistFit<-function(
  x,
  fn,
  params,
  units="deg",        #Units of data: deg or rad
  nBins=15,           #Number of bins
  range=pi,           #Range of data: pi or 2*pi
  circleProp="half",  #Plot on the whole circle or half circle
  xStart=-pi/2,       #Data leftmost limit
  xEnd=pi/2,          #Data righermost limit
  nX=5,               #Number of x grid lines
  xUnit="deg",        #X-axis units: deg or rad
  xLab="Offset",      #X-axis label
  binSize=range/nBins,      #Bin size
  lineColBars="red4",       #Colour of histrogram lines
  lineColFit="black",       #Colour of fit line
  fillCol="palevioletred",  #Colour of histogram fill
  lineSizeBars=1,     #Thickness of histrogram lines
  lineSizeFit=1,      #Thickness of fit lines
  innerRadius=0.5,    #Radius of inner circle
  yIncrement=0.5,     #Y-axis increment
  start=pi)           #Circle start point
  {
  
  #Required packages
  require(ggplot2)
  
  #Most probability density functions require angles in radians
  if (units=="deg") x<-x*pi/180
  
  #x and y (density) coordinates of the fit line
  if (missing(params)) {
    fit<-data.frame("density"=fn(x)+innerRadius,"x"=x)
  } else {  
    fit<-data.frame("density"=fn(params,x)+innerRadius,"x"=x)
  }
  
  #Order fit line by x-coordinate (lowest to highest)
  fit.ordered<-fit[order(fit$x),]
  
  #Histograms
  pdhist<-histxy(x,binSize,innerRadius,xStart,xEnd)
  
  p<-ggplot()
  
  #Bars
  p<-p+geom_rect(data=pdhist,
                 aes(xmin=x.min,
                     xmax=x.max,
                     ymin=y.min,
                     ymax=y.max),
                 fill=fillCol,
                 size=lineSizeBars,
                 colour=lineColBars)
  
  #Fit line
  p<-p+geom_line(d = fit.ordered,
                   aes(x=x,
                       y=density),
                   size=lineSizeFit,
                   colour=lineColFit)
  
  #x-axis
  if (circleProp=="half") {
    lims<-c(xStart*2,xEnd*2)
    brs<-seq(xStart, xEnd, range/(nX-1))
    if (xUnit=="deg") labels<-as.character(brs*180/pi)
    if (xUnit=="rad") labels<-as.character(round(brs,2))
    p<-p+scale_x_continuous(limits=lims,
                            breaks=brs,
                            labels=labels)
  }
  
  if (circleProp=="whole") {
    lims<-c(xStart,xEnd)
    brs<-seq(xStart, xEnd, range/nX)
    if (xUnit=="deg") labels<-as.character(brs*180/pi)
    if (xUnit=="rad") labels<-as.character(round(brs,2))
    p<-p+scale_x_continuous(limits=lims,
                            breaks=brs,
                            labels=labels)
  }
    
  #y-axis
  ymax<-ceiling(max(fit$density)*1/yIncrement)*yIncrement
  brs<-seq(0,ymax,yIncrement)
  guideStart<-innerRadius/yIncrement
  yAdj<-brs-innerRadius
  yAdj[1:guideStart]<-""
  labels<-as.character(yAdj)
  
  p<-p+scale_y_continuous(limits=c(0,ymax),
                          breaks=brs,
                          labels=labels)  
  
  #Axis labels
  strPaste <- function(..., sep=" ") {
    paste(..., sep=sep, collapse=sep)
  }
  
  if (xUnit=="deg") {
   p<-p+labs(x=strPaste(xLab,"(deg)"),
             y="Probability Density",parse=TRUE)
  }
  
  if (xUnit=="rad") {
    p<-p+labs(x=strPaste(xLab,"(rad)"),
              y="Probability Density")
  }
  
  #Theme elements
  p<-p+theme_bw()+
    theme(panel.border=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.minor.x=element_blank(),
          axis.title.y=element_text(hjust=0.95,vjust=0))
  
  p<-p+coord_polar(start=start)
  
  return(p)
}