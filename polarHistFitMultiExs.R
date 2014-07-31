## =============================================================================
## Two examples on how to use polarHistFitMulti.R v.1
## By Christie Haskell, 31-07-2014 (v.1)
## Source:
## =============================================================================


#Half circle, two normal distributions
set.seed(123)
xa<-180-rnorm(500, mean = 90, sd = 20)%%180
xb<-180-rnorm(500, mean = 90, sd = 30)%%180
xc<-180-rnorm(500, mean = 90, sd = 40)%%180

df<-data.frame("group"=rep(c("a","b","c"),each=500),"x"=c(xa,xb,xc))
              
fn<-function(par,x) {
  exp(-0.5*((x-par[1])/par[2])^2)/(sqrt(2*pi)*par[2])
}

params<-cbind(c(pi/2,20*pi/180),c(pi/2,30*pi/180),c(pi/2,40*pi/180))

p<-polarHistFitMulti(df,fn,params,
                     xStart=0,
                     xEnd=pi,
                     start=-pi/2,
                     xLab="Angle"
                     )

p<-p+theme(axis.title.x = element_text(vjust=14))
g = ggplotGrob(p)
grid.newpage()
pushViewport(viewport(height=1, width=1, clip="on"))
grid.draw(g)
grid.rect(x=0,y=0,height=0.9, width=2, gp=gpar(col="white"))