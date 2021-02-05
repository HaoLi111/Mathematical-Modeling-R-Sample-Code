#This R repository is for demonstration of algorithms involved in the book
#Mathematical Modeling (4th Edition) written by Prof. Mark. M. Meerschaert
#coded, edited and tested by Hao Li during Dec. 2018 - Jan. 2019.

#1-1-2
#Preload
library(Deriv)
library(Ryacas)
casOut<-function(y) return(parse(text=y[["text"]]))
#-----------------------------------------------------------------------------
#1) WITH RESPECT TO r
#ASSUMPTIONS
w<-expression(200+5*t)
p<-expression(0.65-r*t)##FACTOR r IS SET AS A VARIABLE IN THIS CASE
C<-expression(0.45*t)
R<-expression(p*w)
P<-expression(R-C)


y<-yacas(P)
(y<-casOut(y))
dydt<-Deriv(y,"t")
Expand(dydt)
yacas("Solve((%)==0,t)")
#yacas("Solve(dydt==0,t)")
#t<-yacas("Solve((%)==0,t)")
t.opti<-expression(-((-200 * r + 2.8)/(-10 * r)))

r=(8:12)*0.001
x=eval({r<-(8:12)*0.001;t.opti})
plot(r,x,type="b",xlab="r",ylab="x")#PLOT1


dxdr<-Deriv(t.opti,"r")
dxdr<-Expand(dxdr)
plot(r,Eval({r=(8:12)*0.001;dxdr}),
     type="b",ylab = 'dxdr')

#Validity(t, for time must be no smaller than 0)
t.opti<-Expand(t.opti)

#detach("package:Ryacas", unload=TRUE)
#detach("package:Deriv", unload=TRUE)