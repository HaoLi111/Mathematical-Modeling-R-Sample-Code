#Ronge-Kutta

#The algorithm - see Chapter 6 Ex 21
#Case -see 5.4 RLC
Update_RT_2Var =function(x,
                 f1,f2,
                 t,h){
  x1=x[1];x2=x[2]
  hh=h/2
  r1 = f1(x1,x2)
  s1 = f2(x1,x2)
  r2 = f1(x1+hh*r1,x2+hh*s1)
  s2 = f2(x1+hh*r1,x2+hh*s1)
  r3 = f1(x1+hh*r2,x2+hh*s2)
  s3 = f2(x1+hh*r2,x2+hh*s2)
  r4 = f1(x1+h*r3,x2+h*s3)
  s4 = f2(x1+h*r3,x2+h*s3)
  x1u = x1+h/6*(r1+2*r2+2*r3+r4)
  x2u = x2+h/6*(s1+2*s2+2*s3+s4)
  c(x1u,x2u)
}
RT_2Var = function(xInit,
                   f1,f2,
                   init,end,h){
  N=as.integer((end-init)/h)
  xdf = matrix(NA,N+1,2)
  xdf[1,] = xInit
  for(i in 2:N){
    x1=xdf[i-1,1];x2=xdf[i-1,2]
    hh=h/2
    r1 = f1(x1,x2)
    s1 = f2(x1,x2)
    r2 = f1(x1+hh*r1,x2+hh*s1)
    s2 = f2(x1+hh*r1,x2+hh*s1)
    r3 = f1(x1+hh*r2,x2+hh*s2)
    s3 = f2(x1+hh*r2,x2+hh*s2)
    r4 = f1(x1+h*r3,x2+h*s3)
    s4 = f2(x1+h*r3,x2+h*s3)
    xdf[i,1] = x1+h/6*(r1+2*r2+2*r3+r4)
    xdf[i,] = x2+h/6*(s1+2*s2+2*s3+s4)
  #t,x1,x2  xdf = c(x1u,x2u)
  }
  t=seq(from=init,to = end,by=h)
  xdf = cbind(t,xdf)
}


#Utilize the functions

#Define the derivatives
x1p = function(x1,x2) x1 - x1^3 - x2
x2p = function(x1,x2) x1#Regardless of x2

Sim = RT_2Var(xInit = c(-1.0,-1.5),
              f1=x1p,f2=x2p,
              init = 0, end = 10, h = .01)
head(Sim)
plot(Sim[,3]~Sim[,2],xlab ='x1',ylab ='x2')