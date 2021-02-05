#This R repository is for demonstration of algorithms involved in the book
#Mathematical Modeling (4th Edition) written by Prof. Mark. M. Meerschaert
#coded, edited and tested by Hao Li during Dec. 2018 - Jan. 2019.

#1-2-2 Multivariable Optimization
#Plot contour with restrictions
#Load results from 1-2-1
#1. SOLVE THE EQUATIONS

(ym=matrix(c((-174),(-144)),ncol=1))
(B=rbind(c((-0.007),(-0.02)),c((-0.02),-0.007)))
x=solve(B,ym)
remove(ym)
remove(B)
x[1]#s
x[2]#t
y<-function(s,t) (339 - 0.01 * s - 0.003 * t) * s + (399-0.004*s-0.01*t) * t - (195 * s + 225 * t + 4e+05)
y(x[1],x[2])

#Visualization
library(plot3D)
x1=seq(from = 0, to = 10000, length.out = 100);x2=seq(from = 0, to = 10000, length.out = 100)
m = mesh(x=x1,y=x2)
z = y(m$x,m$y)
layout(matrix(1:2,1))

contour(x1,x2,z,xlab='x1',ylab='x2')
abline(v=x[1],untf=FALSE)            
abline(h=x[2],untf=FALSE)
title("Contours of f(x1,x2)")
#Add the margins of domain there
abline(h=8000,col = 'red',untf=FALSE)
abline(v=5000,col = 'red',untf=FALSE)
abline(coef=c(10000,-1),col = 'red',untf=FALSE)

persp3D(x1,x2,z)

#3d Visualization with rgl and Plot3Drgl
library(plot3Drgl)
persp3Drgl(x1,x2,z)
grid3d(c('x+','y+','z+'))
grid3d(c('x-','y-','z-'))
sapply(c('x+','y+','z+'),axis3d)
#points3D(c(x[1],x[2],y(x[1],x[2])),col ='purple')
#lines3d(cbind(c(x[1],x[2],y(x[1],x[2])),c(x[1],x[2],0)))
