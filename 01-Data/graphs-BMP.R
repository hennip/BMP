
#PÄIVITTÄISET ENNUSTEJAKAUMAT
#source("data-BMP.r")
#load(file=str_c(pathMain,"01-Projects/BMP/output/run1_koe3.RData"))

#load(file=str_c(pathMain,"01-Projects/BMP/output/run2_koe3.RData"))

chains<-as.mcmc.list(run)

# YMPPI
df<-boxplot.jags.df(chains, "y_new",c(1:n_days))
df<-setNames(df,c("day","q5","q25","q50","q75","q95"))

ggplot(df, aes(day, group=day))+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity")+
  labs(title="YMPPI, päivittäisen lisäyksen ennustejakaumat", x="Päivä", y="Skaalaamaton kaasumäärä")+
  geom_point(data=tmpy,aes(x=day, y=arvo,color=pullo))+
  coord_cartesian(ylim=c(0,50))+
  theme_bw()+
  theme(legend.position="none") # removes legend

# YMPPI: keskiarvot
df<-boxplot.jags.df(chains, "muy",c(1:n_days))
df<-setNames(df,c("day","q5","q25","q50","q75","q95"))

ggplot(df, aes(day, group=day))+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity")+
  labs(title="YMPPI, päivittäisen lisäyksen keskiarvot", x="Päivä", y="Skaalaamaton kaasumäärä")+
  geom_point(data=tmpy,aes(x=day, y=arvo,color=pullo))+
  theme_bw()+
  theme(legend.position="none") # removes legend


# NÄYTTEET
for(i in 1:n_types){
  #i<-1
  df<-boxplot.jags.df2(chains, "x_new[",str_c(i,"]"),1:n_days)%>%
    mutate(NAYTE=ExpName[i])
  ifelse(i>1, df2<-bind_rows(df2,df),df2<-df)
}
df2<-setNames(df2,c("day","q5","q25","q50","q75","q95","NAYTE"))
df<-dat
#df2<-filter(df2, NAYTE=="BJ"| NAYTE=="TT1")
#df<-dat%>%filter(NAYTE=="BJ"| NAYTE=="TT1")

ggplot(df2, aes(day, group=day))+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity")+
  labs(title="NÄYTTEET, päivittäisen lisäyksen ennustejakaumat", x="Päivä", y="Skaalaamaton kaasumäärä")+
  geom_point(data=df,aes(x=day, y=arvo,color=pullo))+
  facet_wrap(~NAYTE, scales="free")+
  #coord_cartesian(ylim=c(0,50))+
  theme_bw()+
  theme(legend.position="none") # removes legend


# NÄYTTEET:keskiarvot
for(i in 1:n_types){
  #i<-1
  df<-boxplot.jags.df2(chains, "mux[",str_c(i,"]"),1:n_days)%>%
    mutate(NAYTE=ExpName[i])
  ifelse(i>1, df2<-bind_rows(df2,df),df2<-df)
}
df2<-setNames(df2,c("day","q5","q25","q50","q75","q95","NAYTE"))

ggplot(df2, aes(day, group=day))+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity")+
  labs(title="NÄYTTEET, päivittäisen lisäyksen keskiarvot", x="Päivä", y="Skaalaamaton kaasumäärä")+
  geom_point(data=dat,aes(x=day, y=arvo,color=pullo))+
  facet_wrap(~NAYTE, scales="free")+
  theme_bw()+
  theme(legend.position="none") # removes legend



#KUMULATIIVISET KERTYMÄT
#df0<-read_xlsx("data/BMP_InnoTrea_Hennille.xlsx", range="C5:AC46", col_names = F)
#source("data-BMP.r")

#YMPPI; skaalaamaton
df<-boxplot.jags.df(chains, "cum_y_new",c(1:n_days))
df<-setNames(df,c("day","q5","q25","q50","q75","q95"))

ggplot(df, aes(day, group=day))+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity")+
  labs(title="YMPPI, kumulatiivinen kertymä, ennustejakaumat", x="Päivä", y="Kumulatiivinen kaasumäärä")+
  geom_point(data=tmpy,aes(x=day, y=cumul,color=pullo))+
  theme_bw()+
  theme(legend.position="none") # removes legend

#YMPPI; skaalattu
df<-boxplot.jags.df(chains, "tulos_ymppi",c(1:n_days))
df<-setNames(df,c("day","q5","q25","q50","q75","q95"))

ggplot(df, aes(day, group=day))+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity")+
  labs(title="YMPPI, skaalattu kumulatiivinen kertymä, ennustejakaumat", 
       x="Päivä", y="Skaalattu kumulatiivinen kaasumäärä")+
  theme_bw()+
  theme(legend.position="none") # removes legend


# NÄYTTEET
for(i in 1:n_types){
  df<-boxplot.jags.df2(chains, "cum_x_new[",str_c(i,"]"),1:n_days)%>%
    mutate(NAYTE=ExpName[i])
  ifelse(i>1, df2<-bind_rows(df2,df),df2<-df)
}
df2<-setNames(df2,c("day","q5","q25","q50","q75","q95","NAYTE"))

ggplot(df2, aes(day, group=day))+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity")+
  labs(title="NÄYTTEET, kumulatiivinen kertymä, ennustejakaumat", x="Päivä", y="Kumulatiivinen kaasumäärä")+
  geom_point(data=dat,aes(x=day, y=cumul,color=pullo))+
  facet_wrap(~NAYTE, scales="free")+
  theme_bw()+
  theme(legend.position="none") # removes legend


# SKAALATUT EROTUKSET
for(i in 1:n_types){
  df<-boxplot.jags.df2(chains, "tulos[",str_c(i,"]"),1:n_days)%>%
    mutate(NAYTE=ExpName[i])
  ifelse(i>1, df2<-bind_rows(df2,df),df2<-df)
}
df2<-setNames(df2,c("day","q5","q25","q50","q75","q95","NAYTE"))

ggplot(df2, aes(day, group=day))+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity")+
  labs(title="Skaalattu erotus, kumulatiivinen", x="Päivä", y="Kumulatiivinen skaalattu erotus")+
  facet_wrap(~NAYTE)+#, scales="free")+
  theme_bw()+
  theme(legend.position="none") # removes legend

filter(df2, day==33)
# laske tn:t pareittain kumpi on suurempi!

for(i in 1:n_types){
  df<-boxplot.jags.df2(chains, "tulos[",str_c(i,"]"),1:n_days)%>%
    mutate(NAYTE=ExpName[i])
  ifelse(i>1, df2<-bind_rows(df2,df),df2<-df)
}

# p1<-c();p2<-c();p3<-c();p4<-c()
# p1<-ifelse(chains[,"tulos[42,1]"][[1]]-chains[,"tulos[42,3]"][[1]]>0,1,0) # P(Mänty KU > Mänty K) = 1
# p2<-ifelse(chains[,"tulos[42,2]"][[1]]-chains[,"tulos[42,4]"][[1]]>0,1,0) # P(Kuusi KU > Kuusi K) = 0.58
# p3<-ifelse(chains[,"tulos[42,5]"][[1]]-chains[,"tulos[42,7]"][[1]]>0,1,0) # P(Mänty PS > Mänty PF) = 0.19
# p4<-ifelse(chains[,"tulos[42,6]"][[1]]-chains[,"tulos[42,8]"][[1]]>0,1,0) # P(Kuusi PS > Kuusi PF) = 0.43
# mean(p1);mean(p2);mean(p3);mean(p4)


# Koe 3

samp<-array(NA, dim=c(9,length(chains[, "muy[1]"][[1]])))
prob<-c()
for(i in 1:9){
    samp[i,]<-ifelse(chains[,"tulos[33,1]"][[1]]-chains[,str_c("tulos[33,",i+1,"]")][[1]]>0,1,0) 

    prob[i]<-mean(samp[i,])
}

1-prob
