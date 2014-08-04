## =============================================================================
## Example using stackedPolarBarChart.R
## By Christie Haskell, 04-08-2014 (v.1)
## Source: 
## =============================================================================

score<-rep(c("A","B","C"),10)
item<-rep(seq(1,10,1),each=3)
family<-c(rep("I",6),
          rep("II",9),
          rep("III",3),
          rep("IV",12))

set.seed(123)
value<-runif(30,1,20)

df<-data.frame(family,item,score,value)

polarBarChart(df)

