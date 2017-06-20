library(tidyverse)
library(caret)
library(doParallel)
library(foreach)
pv=readRDS("pv.rds")

cl=makeCluster(28)
registerDoParallel(cl)

mytrain=function(data){
  
  ## train setting
  tuneL=4
  trcontrol=trainControl(number=25)
  
  traindf=data
  form=as.formula(eff~tempMax+day+sunDuration+pressure+dewPoint+cloudCover)
  
  print("glmnet")
  m1=train(form,data=traindf,method="glmnet",tuneLength = tuneL,trControl = trcontrol)
  
  # print("svm")  ## too slow
  # m2=train(form,data=traindf,method="svmLinear2",tuneLength = tuneL,trControl = trcontrol)
  
  print("cubist")  ## slow-ish
  m3=train(form,data=traindf,method="cubist",tuneLength = tuneL,trControl = trcontrol)
  
  print("bam")
  m4=train(form,data=traindf,method="bam",tuneLength = tuneL,trControl = trcontrol)

  mget(ls(pattern="^m[1-9]+[a-z]*$"))
}


c1modelList=pv%>%
  nest(-cluster1)%>%
  slice(17:18)%>%
  mutate(model=map(data,mytrain))

c2modelList=pv%>%
  nest(-cluster2)%>%
  slice(7:8)%>%
  mutate(model=map(data,mytrain))

mytrim=function(m){
  m%>%
    mutate(model=map(model,function(mm){
      out=mm$m3
      out$trainingData=NULL
      out$pred=NULL
      out
    }),
    data=NULL)
}


c1model=mytrim(c1modelList)
c2model=mytrim(c2modelList)

## check that prediction works on the trimmed object
predict(c1model$model[[1]],newdata=c1modelList$data[[1]]%>%head(3))
predict(c2model$model[[1]],newdata=c1modelList$data[[1]]%>%head(3))

# ## save model
# saveRDS(c1model,file="c1model.rds")
# saveRDS(c2model,file="c2model.rds")


