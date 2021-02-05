#This R repository is for demonstration of algorithms involved in the book
#Mathematical Modeling (4th Edition) written by Prof. Mark. M. Meerschaert
#coded, edited and tested by Hao Li during Dec. 2018 - Jan. 2019.



#Exp5-1 Eigenvalue Methods
#Clear variables
#rm(list = ls())
library(Ryacas)
#x1 = Sym('x1');x2 = Sym('x2')
yacas("f1:=(10/100)*x1-((10/100)/10000)*x1^2-((5/100)/10000)*x1*x2")
yacas("f2:=(25/100)*x2-((25/100)/6000)*x2^2-((25/200)/6000)*x1*x2")


yacas("df1dx1:=D(x1) f1")
yacas("df1dx2:=D(x2) f1")
yacas("df2dx1:=D(x1) f2")
yacas("df2dx2:=D(x2) f2")


yacas("A:={{df1dx1,df1dx2},{df2dx1,df2dx2}}")
yacas('PrettyForm(A)')


yacas("PrettyForm(S)")


yacas("B:=Subst(x1,28000/3)A")
yacas("B:=Subst(x2,4000/3)B")



yacas('PrettyForm(B)')

yacas('E:=CharacteristicEquation(B,x)')
yacas('E:=Simplify(E)')
yacas('PrettyForm(E)')
yacas('lambda:=Solve(E==0,x)')
yacas('PrettyForm(lambda)')