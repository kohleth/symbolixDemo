library(tidyverse)
library(caret)
pv=readRDS("pv.rds")

mytrain=function(data,cluster=1){

  tuneL=1
  trcontrol=trainControl(number=2)
  df=data
  cl=ifelse(cluster==1,'cluster1','cluster2')
  form=as.formula(paste0("eff~",cl,"+icon+tempMax+day+sunDuration+pressure+dewPoint+cloudCover"))
  
  print("glmnet")
  m1=train(form,data=df,method="glmnet",tuneLength = tuneL)
  
  print("svm")
  m2=train(form,data=df,method="svmLinear",tuneLength = tuneL)
  
  print("cubist")
  m3=train(form,data=df,method="cubist",tuneLength = tuneL)
  
  print("bam")
  m4=train(form,data=df,method="bam",tuneLength = tuneL)
  
  mget(ls(pattern="^m[1-9]+[a-z]+$"))
}


c1model=mytrain(pv,cluster=1)
c2model=mytrain(pv,cluster=2)