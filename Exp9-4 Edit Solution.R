#re-edited Mar 2021


#This R repository is for demonstration of algorithms involved in the book
#Mathematical Modeling (4th Edition) written by Prof. Mark. M. Meerschaert
#
#Edited from the original repository on the website of the book before 2018
#https://www.stt.msu.edu/users/mcubed/modeling.html



# Example 9.4: Bombing Run Problem
#
p=0.9*0.5
q=0.6
m=10
N=15
P=seq(1,m+1,1)
PrX=seq(1,m+1,1)
S=0
for (i1 in 1:m+1){
  i=i1-1
  P[i1]=1-(1-p)^(N-i)
  PrX[i1]=dbinom(i, size=m, prob=q)
  S=S+P[i1]*PrX[i1]}
S 


#-----------------------------------------
rm(list=ls())#clear the variables#NOT ENCOURAGED WAY OF CODING



#Calculate minimum N from a given S
library(Ryacas)
p=0.9*0.5
q=0.6
m=10
Pi<-expression(1-(1-p)^(N-i))
Pi<-as.expression(yacas(Pi))
Pi
N<-0
S<-0
while(S<0.99){
  S<-0
  N<-N+1
  for (i in seq(from=0,to=m,by=1)){
    P<-eval({Pi})
    B<-dbinom(i, size=m, prob=q)
    S<-S+P*B
  }
}
remove(i)
N#The minimum N required for S>0.99
#---------------------wrap up a function to calculate S from N
#Calculation of N from S will be time consuming

NfromS2<-function(S.targ){
  
  p=0.9*0.5
  q=0.6
  m=10
  Pi<-function(N,i){
    p=0.9*0.5
    (1-(1-p)^(N-i))
  }
  for (k in seq_along(N)){
    S<-0
    while(S<S.targ[k]){
      S<-0
      N[k]<-N[k]+1
      for (i in seq(from=0,to=m,by=1)){
        P<-Pi(N[k],i)
        B<-dbinom(i, size=m, prob=q)
        S<-S+P*B
      }
      remove(i)
    }
  }
  N
}
#
#---------------------Sensitivity(S)-------

S<-seq(from = 0.86, to = 1 , by = 0.001)#


N<-sapply(S,NfromS2)
plot(N~S,type = 'l')


#A more efficient way to this Sensitivity Analysis may be loop N for S and plot N~S
