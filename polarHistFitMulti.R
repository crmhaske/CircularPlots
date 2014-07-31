## =============================================================================
## Multiple polar histograms with fit lines v.1
## By Christie Haskell, 27-07-2014 (v.1)
## Source: 
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

polarHistFitMulti<-function(
  df,
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
  cols,               #Colour of histrogram and fit lines
  fillCol=rep("white",length(df$group)),    #Colour of histogram fill
  lineSizeBars=rep(1,length(df$group)),     #Thickness of histrogram lines
  lineSizeFit=rep(1,length(df$group)),      #Thickness of fit lines
  innerRadius=0.5,    #Radius of inner circle
  yIncrement=0.5,     #Y-axis increment
  start=pi,           #Circle start point
  legend=TRUE)        #Display legend: TRUE or FALSE           
  {
  
  #Required packages
  require(ggplot2)
  require(colorspace)
  
  #Most probability density functions require angles in radians
  if (units=="deg") df$x<-df$x*pi/180
  
  df2<-split(df,df$group)
  groups<-names(df2)
  
  #If user has not defined colours, choose them from a sequential HCL
  if (missing(cols)) {
    cols<-rainbow_hcl(length(df2), start = 30, end = 300)
    names(cols)<-names(df2)
  }
  
  #x and y (density) coordinates of the fit line
  fit<-list()
  fit.ordered<-list()
  ymax<-c()
  for (i in 1:length(df2)) {
    if (missing(params)) {
      fit[[i]]<-data.frame(df2[[i]],"density"=fn(df2[[i]]$x)+innerRadius)
      fit.ordered[[i]]<-fit[[i]][order(fit[[i]]$x),]
      ymax[i]<-max(fit[[i]]$density)
    } else {
      fit[[i]]<-data.frame(df2[[i]],"density"=fn(params[,i],df2[[i]]$x)+innerRadius)
      fit.ordered[[i]]<-fit[[i]][order(fit[[i]]$x),]
      ymax[i]<-max(fit[[i]]$density)
    }
  }
  
  #Histograms
  for (i in 1:length(df2)) {
    pdhist[[i]]<-histxy(df2[[i]]$x,binSize,innerRadius,xStart,xEnd)
  }
  
  p<-ggplot()
  
  #Bars
  for (i in 1:length(df2)) {
    if (i==1) al<-1
    else al<-0
    p<-p+geom_rect(data=pdhist[[i]],
                   aes(xmin=x.min,
                       xmax=x.max,
                       ymin=y.min,
                       ymax=y.max),
                   colour=cols[i],
                   fill=fillCol[i],
                   size=lineSizeBars[i],
                   alpha=al)
  }
  
  #Fit line
  for (i in 1:length(df2)) {
    p<-p+geom_line(d = fit.ordered[[i]],
                   aes(x=x,
                       y=density),
                   colour=cols[i],
                   size=lineSizeFit[i])
  }
  
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
  ymax<-ceiling(max(ymax)*1/yIncrement)*yIncrement
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
  
  #Ascetics
  p<-p+theme_bw()+
    theme(panel.border=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.minor.x=element_blank(),
          axis.title.y=element_text(hjust=0.95,vjust=0))
  
  p<-p+coord_polar(start=start)
  
  return(p)
}