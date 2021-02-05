#1-1-2
#warnings('off')#Omit the warnings from Ryacas#IT DOES NOT WORK HOWEVER
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
y<-casOut(y)
dydt<-Deriv(y,"t")
Expand(dydt)
yacas("Solve((%)==0,t)")
#yacas("Solve(dydt==0,t)")
#t<-yacas("Solve((%)==0,t)")
t.opti<-expression(-((-200 * r + 2.8)/(-10 * r)))

r=(8:12)*0.001
x=eval({r<-(8:12)*0.001;t.opti})
plot(r,x,type="b",xlab="x($/Day)",ylab="x(Day)")#PLOT1


dxdr<-Deriv(t.opti,"r")
dxdr<-Expand(dxdr)
plot(r,Eval({r=(8:12)*0.001;dxdr}),
     type="b")
title("dxdr")
#Validity(t, for time must be no smaller than 0)
t.opti<-Expand(t.opti)
yacas("Solve(t.opti==0,r)")

#Sensitivity
#-------------------------------------------------------------------------------
#2) WITH RESPECT TO r

#Sen<-function(x,r,atr){
 # re<-casOut(yacas(Deriv(body(x),deparse(substitute(r)))*Eval({assign(r,atr);x})/atr))
 # return(re)
#}
#removeQuote=function(id) substr(id,2,nchar(id)-1)

remove(r)
rat<-expression(r/t.opti)#ratio r/x 
Sen<-expression(rat*dxdr)#Sensitivity
Sen<-yacas(Sen)
Sen<-Expand(Sen)
Sen<-as.expression(Sen)
Sen<-Simplify(Sen,"r")
Eval({r<-0.01;Sen})

#-------------------------------------------------------------------------------
#2) WITH RESPECT TO G

#LOAD ASSUMPTIONS
w<-expression(200+g*t,fun.arg=T)     #FACTOR g IS SET AS A VARIABLE IN THIS CASE
p<-expression(0.65-0.01*t,fun.arg=T)
C<-expression(0.45*t,fun.arg=T)
R<-expression(p*w,fun.arg=T)
P<-expression(R-C,fun.arg=T)

y<-yacas(P)                         #FIND EXPRESSION OF y WITH RESPECT TO t,g
y<-casOut(y)                        #CORRECT THE FORMAT TO "expression"
dydt<-Deriv(y,"t")
Expand(dydt)
yacas("Solve((%)==0,t)")            #TAKE D ERIVATIVE AND SOLVE dydt==0
t.opti<-expression(-((0.65 * g - 2.45)/(-0.02 * g)))#COPY THE RESULT FROM Ryacas
t.opti<-Expand(t.opti)                              #t.opti WITH RESPECT TO g   
t.opti<-as.expression(t.opti)
#t.opti<-Simplify(t.opti,"g")
rat<-expression(g/t.opti) #RATIO r/x 
dxdg<-Deriv(t.opti,"g")
Sen<-expression(rat*dxdg) #DEFINE AND CALCULATE SYMBOLIC EXPRESSION OF SENSITIVITY
Sen<-yacas(Sen)           #THESE ARE IDENTICAL FOR CALCULATIONS OF THE SENSITIVITY
Sen<-Expand(Sen)          #RUNNING THE PROCESSES TOGETHER CAN CAUSE SOME PROBLEMS
Sen<-as.expression(Sen)   #WHICH ARE PROBABLY CAUSED BY THE INCOMPATIBILITY
Sen<-Simplify(Sen,"g")    #BETWEEN  Ryacas AND Deriv. DETAILS ARE TO BE FOUND
Eval({g<-5;Sen})          #EVALUATE NUMERIC VALUE OF SENSITIVITY AT g<-5

detach("package:Ryacas", unload=TRUE)
detach("package:Deriv", unload=TRUE)