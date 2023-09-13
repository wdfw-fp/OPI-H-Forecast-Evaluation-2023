library(tidyverse)
#load data
dat<-read_csv("data/dat.csv") %>% mutate(across(c(lag1_log_SmAdj:UWI.SON),scale))



#This is the model that we would you, fitted to the full data set
##this is the equivalent of ridge regression
mod<-mgcv::gam(log(abundance)~
                 s(year,bs="bs",m=1,k=15)+
              s(lag1_log_JackOPI,bs="re")+ #this puts at L2 penalty on the coefficient
              s(lag1_log_SmAdj,bs="re")+
              s(lag1_NPGO,bs="re")+
              s(lag1_PDO,bs="re")+
              s(WSST_A,bs="re")+
              s(PDO.MJJ,bs="re")+
              s(MEI.OND,bs="re")+
              s(UWI.JAS,bs="re")+
              s(SST.AMJ,bs="re")+
              s(SSH.AMJ,bs="re")+
              s(UWI.SON,bs="re")
              ,data=dat)

#you can see that several of the coefficients shrink to essentially 0
plot(mod)


summary(mod)

# evaluation years
leave<-15
#sliding window of fitting length
slide<-30
#rows of data
max_yr<-nrow(dat)

library(mgcv)
out<-numeric((leave+1))#vector to hold forecasts
for(i in 0:leave){
  
  #this is the version with no ARMA. Can use the mgcv:gamm function to add ARMA errors, but was having some convergence issues
  mod<-mgcv::gam(log(abundance)~
                   # s(year,bs="bs",m=1,k=15)+
                   s(lag1_log_JackOPI,bs="re")+
                   s(lag1_log_SmAdj,bs="re")+
                   s(lag1_NPGO,bs="re")+
                   s(lag1_PDO,bs="re")+
                   s(WSST_A,bs="re")+
                   s(PDO.MJJ,bs="re")+
                   s(MEI.OND,bs="re")+
                   s(UWI.JAS,bs="re")+
                   s(SST.AMJ,bs="re")+
                   s(UWI.SON,bs="re"),
                  # ,select=T,
                  # control=list(
                  #   maxIter=100,
                  #   tolerance=1e-6),
                 # correlation=corAR1(),
                 data=dat[seq(from=max(1,max_yr+i-(slide+leave)),to=max_yr-(leave+1)+i),])
                 # data=dat[seq(from=1,to=max_yr+i-(leave+1),by=1),])
  
  pred=exp(predict(mod,newdata=dat[max_yr-leave+i,]))
  out[i+1]<-pred
}

#obs vs fitted
plot(abundance~year,data=dat[(54-15):54,],type="p",pch=19,col="black",ylim=c(0,2000))
points(2008:2023,out,type="o",col="red",pch=19)

#MAPE
mean((abs(dat$abundance[(54-15):54]-out)/dat$abundance[(54-15):54]),na.rm=T)
#RMSE
sqrt(mean(((dat$abundance[(54-15):54]-dat$abundance[(54-15):54])^2),na.rm=T))

