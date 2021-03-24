# Example 8.6 ARM problem

#1. Read and preview
# read data (change path for your computer)
# Note: The user must download cm1.txt from the book website because I do not have the copyright to host the data in the github repo
cm1=scan("cm1.txt")
n=length(cm1)
t=seq(1,n,1)
# plot data
#par(mfcol=c(2,2))
plot(t,cm1)


#2. Data manipulation
#repeat for Autoregression involving 1:3 previous terms
#(to avoid copying and pasting lag1=...,fit1...)

cm1df = data.frame(t=t,cm1=cm1)
cm1df = within(cm1df,{
  for(i in 1:3) assign(paste0('lag',i),c(rep(NA,i),cm1[1:(n-i)]))#assign lag1:lag3
  remove(i)
})
#for(i in 1:3) assign(paste0('lag',i),c(rep(NA,i),cm1[1:(n-i)]))#assign lag1:lag3

fit1 = lm(cm1~t+lag1,data=cm1df)
fit2 = lm(cm1~t+lag1+lag2,data = cm1df)
fit3 = lm(cm1~t+lag1+lag2+lag3,data = cm1df)

cm1df = within(cm1df,{
  for(i in 1:3) assign(paste0('fit',i),c(rep(NA,i),predict.lm(eval(parse(text = paste0('fit',i))))))#assign lag1:lag3
  remove(i)
})
cm1df =within(cm1df,{
  for(i in 1:3) assign(paste0('res',i),cm1 - get(paste0('fit',i)))#assign lag1:lag3
  remove(i)
})
#cm1df
head(cm1df)
layout(1)
cm1pred = c('cm1','fit1','fit2','fit3')
matplot(x=cm1df[,'t'],y=cm1df[,cm1pred],type = c('h','l','l','l'),lty = 1:4,col =1:4,
        xlab = 't (month)',ylab = 'value')
grid()
legend(0,9,legend = cm1pred,lwd=2,col = 1:4,lty = 1:4)

layout(1:3)
for(i in 1:3) hist(cm1df[,as.character(paste0('res',i))],xlab =  paste0('residual',i),main = paste0('residual',i))
layout(1)
sd_error=numeric(3)
for(i in 1:3) sd_error[i] = sd(cm1df[,as.character(paste0('res',i))],na.rm =T)
plot(c(1:3), sd_error,
     xlab = 'No. of previous terms(complexity)',ylab = 'SD of residual',type = 'b')
#plot shows how residuals get lowered at the expense of greater complexity

#uncomment the code below to avoid copying and pasting
#!!!Note that this is usually not encouraged because it is unintuitive
#(to avoid copying and pasting lag1=...,fit1...)
#for(i in 1:3){
#  indp = paste0('lag',1:i)
#  sapply(indp,paste,sep = "+")
#  assign(paste0('fit',i),lm(as.formula(paste('cm1 ~ t +',indp))))
#}
#  for(i in 1:3) print(summary(eval(parse(text = paste0('fit',i)))))


# plot autocovariance function for these residuals
#require(graphics)
layout(1:3)
for(i in 1:3) acf(cm1df[(i+1):nrow(cm1df),paste0('res',i)])
#acf(e1)


# forecast cm1 for May 1990
for(i in 1:3) {
  assign(paste0('coef',i),coef(get(paste0('fit',i))))
 print(get(paste0('coef',i)))
 }
nf=11#n of forcast
t2=seq(1,n+nf,1)
for(i in 1:3){
  #for every model
  cm1f = c(cm1,numeric(nf))
  #for(k in 1:i){
   #   #for every lag
    #  coeflag = c(coeflag,get(paste0('coef',k))[paste0('lag',k)])
    #}
  for(j in n+1:nf){
    #for every ts term
    

    #cm1flag = NULL
    #for(k in 1:i){
      #for every lag
    cm1flag= cm1f[(j-i):(j-1)]
    cm1f[j] = sum(get(paste0('coef',i))*c(1,t2[j],cm1flag))
  }
  assign(paste0('cm1f',i),cm1f)
  #fit2 = numeric(length(cm1f))
  #for(j in n+i:nf){
  #  fit2[j] = sum(get(paste0('coef',i))*c(1,t2[j],cm1f[(j-i+1):(j)]))
  #}
  assign(paste0('fit2_',i),fit2)
}
layout(1:3)
for(i in 1:3){
  plot(t2,get(paste0('cm1f',i)),ylab=paste0('fit2_',i), type ='b')
  points(t,cm1,col = 'red')
  abline(v = n)
}

layout(1)
cm1pred = c('cm1','fit1','fit2','fit3')
matplot(x=cm1df[,'t'],y=cm1df[,cm1pred],type = c('h','l','l','l'),lty = 1:4,col =1:4,
        xlab = 't (month)',ylab = 'value',
        xlim = c(0,n+nf),
        ylim = c(min(cm1) - 1.5*sd(cm1),
                 (max(cm1)+1.5*sd(cm1))))
grid()
legend(0,max(cm1)+1.2*sd(cm1),legend = cm1pred,lwd=2,col = 1:4,lty = 1:4)
for(i in 1:3){
  lines(t2,get(paste0('cm1f',i)),ylab=paste0('fit2_',i), type ='l',col = i+1)
  #points(t,cm1,col = 'red')
  abline(v = n,col = 'purple',lty = 3)
}
lines(t,cm1,col = 'grey')


for(i in 1:3) {
  print(i)
  print(get(paste0('cm1f',i)))
}

