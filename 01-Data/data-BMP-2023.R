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


koe1<-read_xlsx(paste0(pathMain,"01-Projects/BMP/data/2023/KOE_1.xlsx"), range="B4:AE36", col_names = F, sheet = "Koe1")
koe2<-read_xlsx(paste0(pathMain,"01-Projects/BMP/data/2023/KOE_2.xlsx"), range="B4:X31", col_names = F, sheet = "Koe2")
koe3<-read_xlsx(paste0(pathMain,"01-Projects/BMP/data/2023/KOE_3_mod.xlsx"), range="B4:AG52", col_names = F, sheet = "Koe3", na="")

# Ymppisarjat:
n_days_y<-c()
n_days_y[1]<-dim(koe1)[1]
n_days_y[2]<-dim(koe2)[1]
n_days_y[3]<-26#dim(koe3)[1]# 26 lopullinen n ympille kokeessa 3
n_days_y

df_y1<-koe1[,1:3]
df_y2<-koe2[,1:3]
df_y3<-koe3[1:n_days_y[3],1:2]

nb_y<-c(length(df_y1),length(df_y2),length(df_y3))


tmpy1<-df_y1%>%
  mutate(day=1:n_days_y[1])%>%
  gather(key="pullo", value="arvo", `...1`:`...3`)%>%
  mutate(p=ifelse(pullo=="...1",1,ifelse(pullo=="...2",2,3)))%>%
  mutate(arvo=as.numeric(arvo))%>%
  mutate(NAYTE="Inoculum")%>%
  group_by(p)%>%
  mutate(cumul=cumsum(arvo))

tmpy2<-df_y2%>%
  mutate(day=1:n_days_y[2])%>%
  gather(key="pullo", value="arvo", `...1`:`...3`)%>%
  mutate(p=ifelse(pullo=="...1",1,ifelse(pullo=="...2",2,3)))%>%
  mutate(arvo=as.numeric(arvo))%>%
  mutate(NAYTE="Inoculum")%>%
  group_by(p)%>%
  mutate(cumul=cumsum(arvo))

tmpy3<-df_y3%>%
  mutate(day=1:n_days_y[3])%>%
  gather(key="pullo", value="arvo", `...1`:`...2`)%>%
  mutate(p=ifelse(pullo=="...1",1,ifelse(pullo=="...2",2,1)))%>%
  mutate(arvo=as.numeric(arvo))%>%
  mutate(NAYTE="Inoculum")%>%
  group_by(p)%>%
  mutate(cumul=cumsum(arvo))

#View(tmpy)


# ExpName1<-c("1 Bioj", "2 tammi16", "3 tammi28", "4 tammi60","5 tammi80",
#             "6 saa16","7 saa28","8 saa60", "9 saa80")
# 
# ExpName2<-c("1 Bioj", "2 huhti3", "3 huhti5", "4 huhti7")
# 
# ExpName3<-c("A Bioj", "B turvet1", "C turvet2", "D turvet3",
#            "E puut1","F puut2","G puut3",
#            "H saa1","I saa2","J saa3")

ExpName1<-c("BW1", "1A1", "1A2", "1A3","1A4",
            "1B1","1B2","1B3", "1B4")

ExpName2<-c("BW2", "2A1", "2A2", "2A3")

ExpName3<-c("BW3", "3A1", "3A2", "3A3",
            "3B1","3B2","3B3",
            "3C1","3C2","3C3")

n_types<-c()
n_types[1]<-length(ExpName1)
n_types[2]<-length(ExpName2)
n_types[3]<-length(ExpName3)

# näytesarjat:
n_days_x<-c()
n_days_x[1]<-dim(koe1)[1]
n_days_x[2]<-dim(koe2)[1]

###############################


df_x1<-list()          
df_x1[[1]]<-as.matrix(koe1[,4:6])
df_x1[[2]]<-as.matrix(koe1[,7:9])
df_x1[[3]]<-as.matrix(koe1[,10:12])
df_x1[[4]]<-as.matrix(koe1[,13:15])
df_x1[[5]]<-as.matrix(koe1[,16:18])
df_x1[[6]]<-as.matrix(koe1[,19:21])
df_x1[[7]]<-as.matrix(koe1[,22:24])
df_x1[[8]]<-as.matrix(koe1[,25:27])
df_x1[[9]]<-as.matrix(koe1[,28:30])


tmpx1<-list()
for(i in 1:9){
  tmpx1[[i]]<-as_tibble(df_x1[[i]])%>%
    mutate(NAYTE=ExpName1[i])%>%
    mutate(day=1:n_days_x[1])%>%
    gather(key="pullo", value="arvo",str_c("...",3*i+1):str_c("...",3*i+3))%>%
    #mutate(p=ifelse(pullo==str_c("...",3*i+1),1,ifelse(pullo==str_c("...",3*i+2),2,3)))%>%
    group_by(pullo)%>%
    mutate(cumul=cumsum(arvo))
  
}

dat1<-tmpx1[[1]]%>%
  full_join(tmpx1[[2]], by=NULL)%>%
  full_join(tmpx1[[3]], by=NULL)%>%
  full_join(tmpx1[[4]], by=NULL)%>%
  full_join(tmpx1[[5]], by=NULL)%>%
  full_join(tmpx1[[6]], by=NULL)%>%
  full_join(tmpx1[[7]], by=NULL)%>%
  full_join(tmpx1[[8]], by=NULL)%>%
  full_join(tmpx1[[9]], by=NULL)


###############################
tmp<-c()
for(i in 1:dim(koe2)[2]){
  # laske summa päivästä 10 eteenpäin, korvaa 10. päivä tällä summalla
  tmp[i]<-sum(koe2[10:dim(koe2)[1],i], na.rm = T)
}

koe2_mod<-koe2

koe2_mod[10,]<-t(tmp)
koe2_mod<-koe2_mod[1:10,]  

n_days_x[2]<-dim(koe2_mod)[1];n_days_x

df_x2<-list()          
df_x2[[1]]<-as.matrix(koe2_mod[,4:7])
df_x2[[2]]<-as.matrix(koe2_mod[,9:13])
df_x2[[3]]<-as.matrix(koe2_mod[,15:18])
df_x2[[4]]<-as.matrix(cbind(koe2_mod[,19:20], koe2_mod[22:23]))

b_df_x2<-list()
b_df_x2[[1]]<-c("...4", "...5", "...6", "...7")
b_df_x2[[2]]<-c("...9", "...10", "...11", "...12", "...13")
b_df_x2[[3]]<-c("...15", "...16", "...17", "...18")
b_df_x2[[4]]<-c("...19", "...20", "...22", "...23")

tmpx2<-list()
for(i in 1:4){
  #  i<-2
  tmpx2[[i]]<-as_tibble(df_x2[[i]])%>%
    mutate(NAYTE=ExpName2[i])%>%
    mutate(day=1:n_days_x[2])%>%
    gather(key="pullo", value="arvo",b_df_x2[[i]])%>%
    #mutate(p=ifelse(pullo=="...7",1,ifelse(pullo=="...8",2,3)))
    group_by(pullo)%>%
    mutate(cumul=cumsum(arvo))
  
}

dat2<-tmpx2[[1]]%>%
  full_join(tmpx2[[2]], by=NULL)%>%
  full_join(tmpx2[[3]], by=NULL)%>%
  full_join(tmpx2[[4]], by=NULL)

###############################

tmp<-c()
for(i in 1:dim(koe3)[2]){
# laske summa päivästä 14 eteenpäin, korvaa 14. päivä tällä summalla
  tmp[i]<-sum(koe3[14:dim(koe3)[1],i], na.rm = T)
}

koe3_mod<-koe3
koe3_mod[14,]<-t(tmp)
koe3_mod<-koe3_mod[1:14,]  

n_days_x[3]<-dim(koe3_mod)[1];n_days_x

df_x3<-list()          
df_x3[[1]]<-as.matrix(koe3_mod[,3:5])
df_x3[[2]]<-as.matrix(koe3_mod[,6:8])
df_x3[[3]]<-as.matrix(koe3_mod[,9:11])
df_x3[[4]]<-as.matrix(koe3_mod[,12:14])
df_x3[[5]]<-as.matrix(koe3_mod[,15:17])
df_x3[[6]]<-as.matrix(koe3_mod[,18:20])
df_x3[[7]]<-as.matrix(koe3_mod[,21:23])
df_x3[[8]]<-as.matrix(koe3_mod[,24:26])
df_x3[[9]]<-as.matrix(koe3_mod[,27:29])
df_x3[[10]]<-as.matrix(koe3_mod[,30:32])


tmpx3<-list()
for(i in 1:10){
  #i<-3
  tmpx3[[i]]<-as_tibble(df_x3[[i]])%>%
    mutate(NAYTE=ExpName3[i])%>%
    mutate(day=1:n_days_x[3])%>%
    gather(key="pullo", value="arvo",str_c("...",3*i):str_c("...",3*i+2))%>%
    #mutate(p=ifelse(pullo==str_c("...",3*i),1,ifelse(pullo==str_c("...",3*i+1),2,3)))%>%
    group_by(pullo)%>%
    mutate(cumul=cumsum(arvo))
  
}


dat3<-tmpx3[[1]]%>%
  full_join(tmpx3[[2]], by=NULL)%>%
  full_join(tmpx3[[3]], by=NULL)%>%
  full_join(tmpx3[[4]], by=NULL)%>%
  full_join(tmpx3[[5]], by=NULL)%>%
  full_join(tmpx3[[6]], by=NULL)%>%
  full_join(tmpx3[[7]], by=NULL)%>%
  full_join(tmpx3[[8]], by=NULL)%>%
  full_join(tmpx3[[9]], by=NULL)%>%
  full_join(tmpx3[[10]], by=NULL)

