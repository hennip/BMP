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


df0<-read_xlsx(paste0(pathMain,"01-Projects/BG/data/2018/BMP_InnoTrea_Hennille_dailyincrease.xlsx"), range="C5:AC46", col_names = F)
#df0<-read_xlsx("data/BMP_InnoTrea_Hennille.xlsx", range="C5:AC46", col_names = F)

n_days<-dim(df0)[1]
df_y<-cbind(rep(NA,n_days),df0[,2:3])
#df_y<-df0[,1:3]
n_types<-8

tmpy<-df_y%>%
  mutate(day=1:42)%>%
  #gather(key="pullo", value="arvo",X__1:X__3)%>%
  #mutate(p=ifelse(pullo=="X__1",1,ifelse(pullo=="X__2",2,3)))%>%
  gather(key="pullo", value="arvo",`rep(NA, n_days)`:`...3`)%>%
  mutate(p=ifelse(pullo=="...2",2,ifelse(pullo=="...3",3,1)))%>%
  mutate(arvo=as.numeric(arvo))%>%
  mutate(NAYTE="Inoculum")


df_x1<-as.matrix(df0[,4:6])
df_x2<-as.matrix(df0[,7:9])
df_x3<-as.matrix(df0[,10:12])
df_x4<-as.matrix(cbind(df0[,13],rep(NA,n_days),df0[,15]))#column 14 is removed (contains NA's)
df_x5<-as.matrix(df0[,16:18])
df_x6<-as.matrix(df0[,19:21])
df_x7<-as.matrix(df0[,22:24])
df_x8<-as.matrix(df0[,25:27])


ExpName<-c("Extracted pine bark", "Extracted spruce bark",
           "Pine bark", "Spruce bark", 
           "Separated pyro pine", "Separated pyro spruce",
           "Centrifuged pyro pine", "Centrifuged pyro spruce")


#ExpName<-c("Manty kuori uutettu", "Kuusi kuori uutettu",
#           "Manty kuori", "Kuusi kuori", 
#           "Manty pyro separoitu", "Kuusi pyro separoitu",
#           "Manty pyro fuugattu", "Kuusi pyro fuugattu")

tmpx1<-as.tibble(df_x1)%>%
  mutate(NAYTE=ExpName[1])%>%
  mutate(day=1:42)%>%
  gather(key="pullo", value="arvo",`...4`:`...6`)%>%
  mutate(p=ifelse(pullo=="...4",1,ifelse(pullo=="...5",2,3)))

tmpx2<-as.tibble(df_x2)%>%
  mutate(NAYTE=ExpName[2])%>%
  mutate(day=1:42)%>%
  gather(key="pullo", value="arvo",`...7`:`...9`)%>%
  mutate(p=ifelse(pullo=="...7",1,ifelse(pullo=="...8",2,3)))

tmpx3<-as.tibble(df_x3)%>%
  mutate(NAYTE=ExpName[3])%>%
  mutate(day=1:42)%>%
  gather(key="pullo", value="arvo",`...10`:`...12`)%>%
  mutate(p=ifelse(pullo=="...10",1,ifelse(pullo=="...11",2,3)))

tmpx4<-as.tibble(df_x4)%>%
  mutate(NAYTE=ExpName[4])%>%
  mutate(day=1:42)%>%
  gather(key="pullo", value="arvo",`...13`:`...15`)%>%
  mutate(p=ifelse(pullo=="...13",1,ifelse(pullo=="...15",3,2)))

tmpx5<-as.tibble(df_x5)%>%
  mutate(NAYTE=ExpName[5])%>%
  mutate(day=1:42)%>%
  gather(key="pullo", value="arvo",`...16`:`...18`)%>%
  mutate(p=ifelse(pullo=="...16",1,ifelse(pullo=="...17",2,3)))

tmpx6<-as.tibble(df_x6)%>%
  mutate(NAYTE=ExpName[6])%>%
  mutate(day=1:42)%>%
  gather(key="pullo", value="arvo",`...19`:`...21`)%>%
  mutate(p=ifelse(pullo=="...19",1,ifelse(pullo=="...20",2,3)))

tmpx7<-as.tibble(df_x7)%>%
  mutate(NAYTE=ExpName[7])%>%
  mutate(day=1:42)%>%
  gather(key="pullo", value="arvo",`...22`:`...24`)%>%
  mutate(p=ifelse(pullo=="...22",1,ifelse(pullo=="...24",2,3)))

tmpx8<-as.tibble(df_x8)%>%
  mutate(NAYTE=ExpName[8])%>%
  mutate(day=1:42)%>%
  gather(key="pullo", value="arvo",`...25`:`...27`)%>%
  mutate(p=ifelse(pullo=="...25",1,ifelse(pullo=="...27",2,3)))

tmpx<-tmpx1%>%
  full_join(tmpx2, by=NULL)%>%
  full_join(tmpx3, by=NULL)%>%
  full_join(tmpx4, by=NULL)%>%
  full_join(tmpx5, by=NULL)%>%
  full_join(tmpx6, by=NULL)%>%
  full_join(tmpx7, by=NULL)%>%
  full_join(tmpx8, by=NULL)%>%
  full_join(tmpy, by=NULL)


