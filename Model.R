library(tseries)
library("sandwich")
library("lmtest")

setwd("C:/Users/LENOVO/Desktop/Bioes Bioestatistica/Projeto")

data_read<-read.csv("hipertensao.csv",header = TRUE,sep = ";")
head(data_read)


#order data by the date
data_read<-data_read[order(data_read[,1]),]
dim(data_read)


data_mean<-matrix(NA,nrow=10000,ncol=4)
k<-1
#compute the sum of the patients and the mean of the porportion by ars, in every month
for (i in unique(data_read$Per√.odo)){
  for(j in unique(data_read$Regi√.o)){
    
    data_mean[k,1]<-i
    data_mean[k,2]<-j
    data_mean[k,3]<-sum(data_read[data_read$Per√.odo==i & data_read$Regi√.o==j,5])
    data_mean[k,4]<-mean(data_read[data_read$Per√.odo==i & data_read$Regi√.o==j,6])
    k<-k+1
  }
}

data_mean<- as.data.frame(data_mean)
data_mean<-data_mean[!is.na(data_mean$V2),]



data_mean[data_mean$V3=="NaN",3]<-NA
data_mean[data_mean$V4=="NaN",4]<-NA

#na's correction (onyl 2 null values)
for ( i in unique(data_read$Regi√.o)){
  data_mean[data_mean$V2==i,3]<-na.locf(ts(data_mean[data_mean$V2==i,3]))
  
}

for ( i in unique(data_read$Regi√.o)){
  data_mean[data_mean$V2==i,4]<-na.locf(ts(data_mean[data_mean$V2==i,4]))
  
}


#definition of a binary variable, to indicate if the observation was during or before covid
data_mean[data_mean$V1<"2020-04",1]<-"0"
data_mean[data_mean$V1>="2020-04",1]<-"1"


#estimation of the models using the number of patients
for ( i in unique(data_read$Regi√.o)){
  
  print(i)
  model<-lm(formula = "V3 ~ V1",data = (data_mean[data_mean$V2==i,]))
  
  print("Stationary test:")
  print(pp.test((residuals(model))))
  
  #autocorrelation robust covariance estimator
  model_aux<-coeftest(model, vcov.=NeweyWest(model, adjust=TRUE,verbose=FALSE),df = 0)
  #df=0-> defaults to z-tests
  
  
  print("coefs")
  print(round(coef(model_aux),3))
  print("Confidence intervals")
  print(round(confint(model_aux),3))
  
  print("Estimated decresase")
  coef=as.numeric(coef(model_aux))
  ratio=coef[2]/coef[1]
  print(round(ratio,3))
  
  print("CI")
  vcov.=NeweyWest(model, adjust=TRUE,verbose=FALSE)
  se=coef[1]^-1 * sqrt(vcov.[2,2]+vcov.[1,1]*ratio^2-2*ratio*vcov.[1,2])
  print(round(ratio-1.96*se,3))
  print(round(ratio+1.96*se,3))
  
  
  
}


#estimation of the models using the porportion of the patients
for ( i in unique(data_read$Regi√.o)){
  
  print(i)
  model<-lm(formula = "V4 ~ V1",data = (data_mean[data_mean$V2==i,]))
  
  print("Stationary test:")
  print(pp.test((residuals(model))))
  
  #autocorrelation robust covariance estimator
  model_aux<-coeftest(model, vcov.=NeweyWest(model, adjust=TRUE,verbose=FALSE),df = 0)
  #df=0-> defaults to z-tests

  print("coefs")
  print(round(coef(model_aux),3))
  print("Confidence intervals")
  print(round(confint(model_aux),3))
  print("Estimated decresase")
  coef=as.numeric(coef(model_aux))
  ratio=coef[2]/coef[1]
  print(round(ratio,3))
  
  print("CI")
  vcov.=NeweyWest(model, adjust=TRUE,verbose=FALSE)
  se=coef[1]^-1 * sqrt(vcov.[2,2]+vcov.[1,1]*ratio^2-2*ratio*vcov.[1,2])
  print(round(ratio-1.96*se,3))
  print(round(ratio+1.96*se,3))
  
}

