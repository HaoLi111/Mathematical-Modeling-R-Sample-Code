#This R repository is for demonstration of algorithms involved in the book
#Mathematical Modeling (4th Edition) written by Prof. Mark. M. Meerschaert
#coded, edited and tested by Hao Li during Dec. 2018 - Jan. 2019.



#Exp5-1 Eigenvalue Methods
#Clear variables
#rm(list = ls())
library(Ryacas)
#x1 = Sym('x1');x2 = Sym('x2')
yac("f1:=(10/100)*x1-((10/100)/10000)*x1^2-((5/100)/10000)*x1*x2")
yac("f2:=(25/100)*x2-((25/100)/6000)*x2^2-((25/200)/6000)*x1*x2")


yac("df1dx1:=D(x1) f1")
yac("df1dx2:=D(x2) f1")
yac("df2dx1:=D(x1) f2")
yac("df2dx2:=D(x2) f2")


yac("A:={{df1dx1,df1dx2},{df2dx1,df2dx2}}")
yac('PrettyForm(A)')


yac("PrettyForm(S)")


yac("B:=Subst(x1,28000/3)A")
yac("B:=Subst(x2,4000/3)B")



yac('PrettyForm(B)')

yac('E:=CharacteristicEquation(B,x)')
yac('E:=Simplify(E)')
yac('PrettyForm(E)')
yac('TexForm(E)')
yac('lambda:=Solve(E==0,x)')
yac('PrettyForm(lambda)')

