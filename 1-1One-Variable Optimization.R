#re-edited Mar 2021


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
library(Ryacas)
w<-expression(200+5*t1)
yac_assign(w,"w")
p<-expression(0.65-0.01*t1)
yac_assign(p,"p")
C<-expression(0.45*t1)
yac_assign(C,"C")
R<-expression(p*w)
yac_assign(R,"R")
P<-expression(R-C)


yac_assign(P,"P")
P = yac("P",rettype = "expr")
P




dPdt = yac(paste0("D(","t1",")", as.character(P)),rettype = "expr")
d2Pdt2 = yac(paste0("D(","t1",")", as.character(dPdt)),rettype = "expr")
sln = yac(paste0("Solve(",dPdt,",t1)"),rettype = "str")
sln
l = nchar(sln)
l
sln_str = gsub("==","=",substr(sln,2,l-1))


sln_expr = parse(text = sln_str)
sln_expr
eval(sln_expr)
(P.max  = eval(P))
eval(dPdt)
eval(d2Pdt2)

#-------------------------------------------------------------------------
plot(0:20,eval({t1=0:20;P}),type="l",xlab="Time(D)",ylab="Profit($)")
title("Profit~Time")
abline(v=8,untf=FALSE)
#------------------------------------------------------------------------
sln_expr = parse(text = sln_str)
sln_expr
eval(sln_expr)
t.opti=t1


P.max=eval({t1<-t.opti;P})
list("t.opti"=t.opti,"P.max"=P.max)

#detach("package:Ryacas", unload=TRUE)
#detach("package:Deriv", unload=TRUE)
