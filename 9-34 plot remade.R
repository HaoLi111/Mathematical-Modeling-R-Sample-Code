#Brownian motion problem revisited with new visualization mappings
library(ggplot2)


set.seed(123)

#prepare data 2D
n=64
n_step = 128
tune_mean = 0
tune_sd = 1

xyDf = NULL

for(i in 1:n){
  xyDf=rbind(xyDf,data.frame(t = 1:n_step,x = cumsum(rnorm(n_step,mean = tune_mean,sd = tune_sd)),y = cumsum(rnorm(n_step,mean = tune_mean,sd = tune_sd)),n = rep(i,n_step)))
}
xyDf$n = factor(xyDf$n)

g = ggplot(data = xyDf,aes(x = x,y = y,group =n,color = n)) + geom_point(alpha = .3,size = .2) +geom_path(aes(alpha = -t)) + theme(legend.position = "none")#+ scale_color_distiller(palette = "Spectral")
g

g2 = ggplot(data = xyDf[xyDf$n==1,],aes(x = x,y = y,alpha = -t)) + geom_point(size = .2) +geom_path() + theme(legend.position = "none")#+ scale_color_distiller(palette = "Spectral")
g2

