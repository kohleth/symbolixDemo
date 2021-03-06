library(foreach)
library(tidyverse)
library(latticeExtra)
library(lubridate)
load("VICdata.RData")

pvdata=foreach(ff=list.files("pvdata/",full.names = TRUE),.combine=rbind)%do%{
  read_csv(ff)%>%
    mutate(date=as.Date(gsub(".csv","",basename(ff)),format="%Y%m%d"))
}

weatherdata=foreach(ff=list.files("weather/",full.names = TRUE),.combine=plyr::rbind.fill)%do%{
  read_csv(ff)%>%select(-X1)
}

alldata=pvdata%>%
  left_join(weatherdata,by=c("date"="time","postcode"="postcode"))%>%
  mutate(sunDuration=sunsetTime-sunriseTime,
         tempMin=(temperatureMin-32)/1.8,
         tempMax=(temperatureMax-32)/1.8,
         eff=output/size)%>%
  select(id,postcode,eff,date,icon,sunDuration,precipType,tempMin,tempMax,dewPoint,humidity,cloudCover,pressure)

## inspect missingness
alldata%>%
  summarize_each(funs(mean(is.na(.)|is.null(.))))%>%glimpse

summary(alldata)

## explorartory graphs
xyplot(eff~tempMax,data=alldata,type=c("g","smooth"),auto.key=T)
xyplot(eff~date,type=c("g","smooth"),data=alldata)
bwplot(reorder(icon,eff)~eff,data=alldata)
xyplot(eff~as.numeric(sunDuration),data=alldata,type=c("g","smooth"))
xyplot(eff~pressure,data=alldata,type=c("g","smooth"))
xyplot(eff~dewPoint,data=alldata,type=c("g","smooth"))
xyplot(eff~cloudCover,data=alldata,type=c("g","smooth"))


## cluster postcode -----------------------------
Ncluster=20
doNotCluster=c(3802,3761,3698)  ## postcodes that I want to model explicitly. (i.e. no clustering)

## data for postcode clustering
d0=alldata%>%
  select(eff,postcode,id,tempMax,date,sunDuration,pressure,dewPoint,cloudCover,icon)%>%
  na.omit()%>%
  mutate(day=yday(date),
         sunDuration=as.numeric(sunDuration),
         icon=factor(icon))


## cluster 1 - using weather information
d1=d0%>%
  select(-eff,-id,-date)%>%
  mutate(icon=as.numeric(factor(icon)))%>%
  group_by(postcode)%>%
  summarize_each(funs(mean))%>%
  ungroup%>%
  filter(!postcode%in%doNotCluster)

km1=kmeans(d1%>%select(-postcode),centers = Ncluster,nstart=50,iter.max=10,algorithm = "Hartigan-Wong")

cluster1=d1%>%
  select(postcode)%>%
  mutate(cluster=km1$cluster)%>%
  bind_rows(data.frame(postcode=doNotCluster,cluster=Ncluster+(1:length(doNotCluster))))

## cluster 2 - based on lat/lon, if weather information (cluster 1) not available
d2=d0%>%
  distinct(postcode)%>%
  left_join(VICdata%>%
              select(postcode,lat,lon)%>%
              group_by(postcode)%>%
              summarize_each(funs(mean)))%>%
  filter(!postcode%in%doNotCluster)

km2=kmeans(d2%>%select(lat,lon),centers = Ncluster,nstart=50,iter.max=10,algorithm = "Hartigan-Wong")

cluster2=d2%>%
  mutate(cluster=km2$cluster)%>%
  bind_rows(data.frame(postcode=doNotCluster,cluster=Ncluster+(1:length(doNotCluster))))

## data for modelling
pv=d0%>%
  left_join(cluster1)%>%
  left_join(cluster2,by = "postcode")%>%
  mutate(cluster1=factor(cluster.x),
         cluster2=factor(cluster.y),
         cluster.x=NULL,
         cluster.y=NULL)

saveRDS(pv,file="pv.rds")
