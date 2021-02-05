#This R repository is for demonstration of algorithms involved in the book
#Mathematical Modeling (4th Edition) written by Prof. Mark. M. Meerschaert
#coded, edited and tested by Hao Li during Dec. 2018 - Jan. 2019.

#Exp 3.2
#Discrete Optimization Problem

#R script to solve Exp 3.2 with NO SIMPLIFICATION ON THE ORIGIONAL MAP
#This uses some R conventions with origin on the top left corner of the matrix
#
#Define map matrix

gmap = cbind(c(3,2,5,8,10,0),
             c(0,1,3,5,6,2),
             c(1,1,3,2,3,3),
             c(4,2,0,1,1,1),
             c(2,3,1,0,3,1),
             c(1,2,2,0,1,1))
#gmap = edit(gmap)#Uncomment this to edit the 'map' in an e-spreadsheet

require(plot3D)

x = seq(from = .5,to=5.5,by=1);y = seq(from = .5,to = 5.5,by=1)
gbase = mesh(x,y)
#gbase$x
#gbase$y

#Define radius function r
r = function(pos,gbase) sqrt((pos[1]-gbase$x)^2 + (pos[2]-gbase$y)^2)
#This uses  vectorizations for many times, returns a radius matrix for 
#position marked at pos on geographical base

ztime= function(pos,gmap,gbase) 3.2 + 1.7* sum(gmap* r(pos,gbase)^.91)/84

library(doParallel)
#registerDoParallel(8)#Uncomment this to activate parallel computing

pix = .05
xbase = seq(from = 0, to =6, by = pix);ybase = xbase

z = foreach(i = seq_along(ybase),.combine = cbind) %dopar%{
  v= numeric(length(xbase))
  for(j in seq_along(xbase)) v[j]<-ztime(pos =c(xbase[j],ybase[i]),gmap,gbase)
  v
}

persp(z)

library(plot3Drgl)
persp3Drgl(xbase,ybase,z)
grid3d(c('x+','y+','z+'))
grid3d(c('x-','y-','z-'))
sapply(c('x+','y+','z+'),axis3d)



filled.contour(xbase,ybase,z)
contour(xbase,ybase,z)

#which.min(z)/length(xbase)
yi =as.integer(which.min(z)/length(xbase))
(x_min_trans = ybase[yi])
xi = which.min(z)- yi*length(xbase)
(y_min_trans = 6-xbase[xi])
(z_min_trans = min(z))

#Alternatively...
#Random search test
re=foreach(i = 1:8,.combine = cbind) %dopar% {
  R=Inf
  for(j in 1:125){
    pos=runif(2,0,6)
    Rnew =ztime(pos,gmap,gbase)
    if(Rnew<R){
      p = pos
      R = Rnew
    }
  }
  rbind(p[1],p[2],R)
}
result=re[,which.min(re[3,])]


(x_min_trans1 = result[2])
(y_min_trans1 = 6-result[1])
(z_min_trans1 = result[3])


      