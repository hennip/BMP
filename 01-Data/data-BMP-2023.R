library(rjags)
library(runjags)
load.module("mix")
#library(xlsx)
library(tidyverse)
library(ggmcmc)
library(readxl)
library(forcats)
library(lubridate)
library(stringr)
require(gridExtra)

source("tidy-functions.r")
source("C:/tmp/path-main.r")


#df0<-read_xlsx(paste0(pathMain,"01-Projects/BMP/data/2018/BMP_InnoTrea_Hennille_dailyincrease.xlsx"), range="C5:AC46", col_names = F)
df0<-read_xlsx(paste0(pathMain,"01-Projects/BMP/data/2023/KOE_3.xlsx"), range="B4:AG36", col_names = F, sheet = "Koe3")

n_days<-dim(df0)[1];n_days
#df_y<-cbind(rep(NA,n_days),df0[,2:3])
df_y<-df0[,1:2]
n_types<-10

tmpy<-df_y%>%
  mutate(day=1:n_days)%>%
  gather(key="pullo", value="arvo", `...1`:`...2`)%>%
  mutate(p=ifelse(pullo=="...1",1,ifelse(pullo=="...2",2,1)))%>%
  mutate(arvo=as.numeric(arvo))%>%
  mutate(NAYTE="Inoculum")%>%
  group_by(p)%>%
  mutate(cumul=cumsum(arvo))
#View(tmpy)

ExpName<-c("BJ", "TT1", "TT2", "TT3",
           "PT1","PT2","PT3",
           "S1","S2","S3")
df_x<-list()          
df_x[[1]]<-as.matrix(df0[,3:5])
df_x[[2]]<-as.matrix(df0[,6:8])
df_x[[3]]<-as.matrix(df0[,9:11])
df_x[[4]]<-as.matrix(df0[,12:14])
#df-x[[4]]<-as.matrix(cbind(df0[,13],rep(NA,n_days),df0[,15]))#column 14 is removed (contains NA's)
df_x[[5]]<-as.matrix(df0[,15:17])
df_x[[6]]<-as.matrix(df0[,18:20])
df_x[[7]]<-as.matrix(df0[,21:23])
df_x[[8]]<-as.matrix(df0[,24:26])
df_x[[9]]<-as.matrix(df0[,27:29])
df_x[[10]]<-as.matrix(df0[,30:32])


tmpx<-list()
for(i in 1:10){
#i<-3
  tmpx[[i]]<-as_tibble(df_x[[i]])%>%
  mutate(NAYTE=ExpName[i])%>%
  mutate(day=1:n_days)%>%
  gather(key="pullo", value="arvo",str_c("...",3*i):str_c("...",3*i+2))%>%
  mutate(p=ifelse(pullo==str_c("...",3*i),1,ifelse(pullo==str_c("...",3*i+1),2,3)))%>%
    group_by(pullo)%>%
    mutate(cumul=cumsum(arvo))
  
}



dat<-tmpx[[1]]%>%
  full_join(tmpx[[2]], by=NULL)%>%
  full_join(tmpx[[3]], by=NULL)%>%
  full_join(tmpx[[4]], by=NULL)%>%
  full_join(tmpx[[5]], by=NULL)%>%
  full_join(tmpx[[6]], by=NULL)%>%
  full_join(tmpx[[7]], by=NULL)%>%
  full_join(tmpx[[8]], by=NULL)%>%
  full_join(tmpx[[9]], by=NULL)%>%
  full_join(tmpx[[10]], by=NULL)#%>%
  #full_join(tmpy, by=NULL)


