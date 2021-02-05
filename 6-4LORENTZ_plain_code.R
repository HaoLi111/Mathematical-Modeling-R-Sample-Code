#This R repository is for demonstration of algorithms involved in the book
#Mathematical Modeling (4th Edition) written by Prof. Mark. M. Meerschaert
#coded, edited and tested by Hao Li during Dec. 2018 - Jan. 2019.

#Simulation of Loerez System
#Reference: Mathematic Modelling 4th Ed.

x1p = function(x1,x2,x3,Sigma) -Sigma*x1+Sigma*x2
x2p = function(x1,x2,x3,r) -x2+r*x1-x1*x3
x3p = function(x1,x2,x3,b) -b*x3+x1*x2

#Using Euler s Method

xLorentz = function(x,dt,Sigma,r,b){
  c(x[1]+dt*x1p(x[1],x[2],x[3],Sigma = Sigma),
    x[2]+dt*x2p(x[1],x[2],x[3],r=r),
    x[3]+dt*x3p(x[1],x[2],x[3],b=b))
}                                
Sigma = 10
b = 8/3
#Initial condition
xi=  c(1,1,1)
r = 8

#t Domain
t = seq(from = 0, to = 5, length.out = 1001)
(dt = t[2] - t[1])


Sim1 = matrix(NA,length(t),3)#x1,x2,x3 then cbind t to the left
Sim1[1,] = xi
system.time({
for(i in seq_along(t)[-1]){
  Sim1[i,] =xLorentz(Sim1[i-1,],dt,Sigma,r,b)
}
})
Sim1 = cbind(t,Sim1)

#Sim1

layout(matrix(1:4,2,2))
plot(Sim1[,1],Sim1[,2],
     xlab = 't',
     ylab = 'x1',type = 'l')
plot(Sim1[,1],Sim1[,3],
     xlab = 't',
     ylab ='x2',type = 'l')
plot(Sim1[,1],Sim1[,4],
     xlab = 't',
     ylab ='x3',type = 'l')
#require package: scatterplot3d
#scatterplot3d::scatterplot3d(x = Sim1[,2],y = Sim1[,3],z = Sim1[,4],
         #                    xlab = 'x1',ylab = 'x2',zlab = 'x3')

#require package: plot3D
#windows()#if using mac type macintosh(), open new R graphic device
plot3D::scatter3D(x = Sim1[,2],
                        y = Sim1[,3],
                        z = Sim1[,4],
                        colvar =Sim1[,1],add = F)
layout(1)
plot(Sim1[,2],Sim1[,3],xlab = 'x1',ylab = 'x2',type = 'l')
grid()
#require package: plot3Drgl
plot3Drgl::scatter3Drgl(x = Sim1[,2],
                       y = Sim1[,3],
                       z = Sim1[,4],
                       colvar =Sim1[,1],add = F)

grid3d(c('x+','y+','z+'))
grid3d(c('x-','y-','z-'))
sapply(c('x+','y+','z+'),axis3d)