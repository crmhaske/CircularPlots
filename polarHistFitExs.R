## =============================================================================
## Two examples on how to use polarHistFit.R v.1
## By Christie Haskell, 27-07-2014 (v.1)
## Source: 
## =============================================================================


#Half circle, bimodal distribution
set.seed(123)
x <- c(rnorm(500,50,10),rnorm(500,120,10))
fit<-normalmixEM(x*pi/180)
params<-c(fit$lambda[1],fit$mu[1],fit$sigma[1],fit$lambda[2],fit$mu[2],fit$sigma[2])

fn<-function(par,x) {
  par[1]*exp(-0.5*((x-par[2])/par[3])^2)/(sqrt(2*pi)*par[3])+
    par[4]*exp(-0.5*((x-par[5])/par[6])^2)/(sqrt(2*pi)*par[6])
}

p1<-polarHistFit(x=x,
             fn=fn,
             params,
             xStart=0,
             xEnd=pi,
             start=-pi/2,
             xLab="Angle")
p1<-p1+theme(axis.title.x = element_text(vjust=14))
g = ggplotGrob(p1)
grid.newpage()
pushViewport(viewport(height=1, width=1, clip="on"))
grid.draw(g)
grid.rect(x=0,y=0,height=0.9, width=2, gp=gpar(col="white"))

#Whole circle, normal distribution
set.seed(123)
x2<-rnorm(500, mean = 180, sd = 50)
fn2<-function(par,x) {
  exp(-0.5*((x-par[1])/par[2])^2)/(sqrt(2*pi)*par[2])
}

polarHistFit(x2,fn2,c(pi,50*pi/180),
             circleProp="whole",
             range=2*pi,
             xStart=0,
             xEnd=2*pi,
             nBins=20,
             xLab="Angle",
             innerRadius=0.2,
             yIncrement=0.2)
