#1-1-2
#warnings('off')#Omit the warnings from Ryacas#IT DOES NOT WORK HOWEVER
#Preload
library(Deriv)
library(Ryacas)
casOut<-function(y) return(parse(text=y[["text"]]))
#-----------------------------------------------------------------------------
#1) WITH RESPECT TO r
#ASSUMPTIONS

library(Ryacas)
w<-expression(200+5*t1)
yac_assign(w,"w")
p<-expression(0.65-r*t1)##FACTOR r IS SET AS A VARIABLE IN THIS CASE
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

t1_opt = parse(text = gsub("}"," ",gsub("{t1=="," ",sln,fixed = TRUE),fixed = TRUE))
t1_opt = yac_expr(t1_opt)
t1_opt #solution for optimal value for t w.r.t. r
P_opt = gsub("t1",as.character(t1_opt),as.character(P),fixed = TRUE)
P_opt = parse(text = P_opt)
P_opt
#Above repeats 1-1 but with a variable r instead of constants, get symbolic expression sln_expr

r=(8:12)*0.001
x=eval({r<-(8:12)*0.001;t1_opt})
plot(r,x,type="b",xlab="r($/Day)",ylab="x(Day)")#PLOT1


dxdr<- yac(paste("D(","r",")", as.character(t1_opt)),rettype = "expr")
dxdr
yac_assign(dxdr,"dxdr")
S = expression(dxdr*r/x)
S = yac_assign(S,"S")
S = yac("Simplify(S)",rettype = "expr")
S



#-------------------------------------------------------------------------------
#2) WITH RESPECT TO G

#LOAD ASSUMPTIONS

detach("package:Ryacas", unload = TRUE)
#clear symbolic vars

require(Ryacas)
w<-expression(200+g*t1)
yac_assign(w,"w")#FACTOR g IS SET AS A VARIABLE IN THIS CASE
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

t1_opt = parse(text = gsub("}"," ",gsub("{t1=="," ",sln,fixed = TRUE),fixed = TRUE))
t1_opt = yac_expr(t1_opt)
t1_opt #solution for optimal value for t w.r.t. g
P_opt = gsub("t1",as.character(t1_opt),as.character(P),fixed = TRUE)
P_opt = parse(text = P_opt)
P_opt
#Above repeats 1-1 but with a variable r instead of constants, get symbolic expression sln_expr

g=3:7
x=eval({g<-3:7;t1_opt})
plot(g,x,type="b",xlab="g(lbs/Day)",ylab="x(Day)")#PLOT1


dxdg<- yac(paste("D(","g",")", as.character(t1_opt)),rettype = "expr")
yac_assign(dxdr,"dxdg")
S = expression(dxdr*g/x)
S = yac_assign(S,"S")
S = yac("Simplify(S)",rettype = "expr")
S


