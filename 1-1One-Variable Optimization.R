#This R repository is for demonstration of algorithms involved in the book
#Mathematical Modeling (4th Edition) written by Prof. Mark. M. Meerschaert
#coded, edited and tested by Hao Li during Dec. 2018 - Jan. 2019.

#1-1-1
#1 var optimization Symbolic&Numeric calculations and visualization with R

#
#1 Variable Optimization sample question
#-------------------------------------------------------------------------
#AIM: FIND P MAX
#ASSUMPTIONS
w<-expression(200+5*t)
p<-expression(0.65-0.01*t)
C<-expression(0.45*t)
R<-expression(p*w)
P<-expression(R-C)
#t<-expression(t>0)
#load symbolic calculation package
library(Ryacas)
#library(Deriv)
#Subtitute and expand P(R,c) and get P(t)
yacas(P)##DO NOT RUN expression((0.65 - 0.01 * t) * (5 * t + 200) - 0.45 * t)
y<-yacas(P)
y<-y[["text"]]
y<-parse(text=y)
dydt=D(y,"t")#Find the 1st Derivative
              #dydt<-parse(text=dydt)
              #<-parse(text=paste(as.character(dydt)[2],as.character(dydt)[3]))
d2ydt2<-D(dydt,"t")#2nd deriv
#-------------------------------------------------------------------------
#Display Results
y

dydt

d2ydt2

#> dydt
#(0.65 - 0.01 * t) * 5 - 0.01 * (5 * t + 200) - 0.45
#Copy the result of dydt and y
yacas("(0.65 - 0.01 * t) * 5 - 0.01 * (5 * t + 200) - 0.45")#dydt==0
#yacas("Simplify(%)")
yacas("Solve((%)==0,t)")
yacas("Solve((0.65 - 0.01 * t) * (5 * t + 200) - 0.45 * t==0,t)")#y==0
##t.opti=0.8/0.1=8
#-------------------------------------------------------------------------
plot(0:20,eval({t=0:20;y}),type="l",xlab="Time(D)",ylab="Profit($)")
title("Profit~Time")
abline(v=8,untf=FALSE)
#------------------------------------------------------------------------
t.opti=8
P.max=eval({t<-t.opti;y})
list("t.opti"=t.opti,"P.max"=P.max)

#detach("package:Ryacas", unload=TRUE)
#detach("package:Deriv", unload=TRUE)