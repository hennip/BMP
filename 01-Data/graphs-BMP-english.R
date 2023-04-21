
#PÄIVITTÄISET ENNUSTEJAKAUMAT
df0<-read_xlsx("data/BMP_InnoTrea_Hennille_dailyincrease.xlsx", range="C5:AC46", col_names = F)
source("data-BMP.r")


# YMPPI
df1<-boxplot.jags.df(chains, "y_new",c(1:n_days))
df1<-setNames(df1,c("day","q5","q25","q50","q75","q95"))
df1<-df1%>%mutate(NAYTE="Inoculum")

# NÄYTTEET (ei pyro)
#for(i in 1:n_types){
for(i in 1:4){
  #i<-1
  df<-boxplot.jags.df2(chains, "x_new[",str_c(i,"]"),1:n_days)%>%
    mutate(NAYTE=ExpName[i])
  ifelse(i>1, df2<-bind_rows(df2,df),df2<-df)
}
df2<-setNames(df2,c("day","q5","q25","q50","q75","q95","NAYTE"))

df<-full_join(df1,df2)
df<-as.tibble(df)%>%
#  mutate(NAYTE2=parse_factor(NAYTE,levels=c("Inoculum","Extracted pine bark", "Extracted spruce bark",
#                                            "Pine bark", "Spruce bark", 
#                                            "Separated pyro pine", "Separated pyro spruce",
#                                            "Centrifuged pyro pine", "Centrifuged pyro spruce"
                                            
    mutate(NAYTE2=parse_factor(NAYTE,levels=c("Inoculum","Extracted pine bark", "Extracted spruce bark",
                                            "Pine bark", "Spruce bark"#, 
                                            #"Separated pyro pine", "Separated pyro spruce",
                                            #"Centrifuged pyro pine", "Centrifuged pyro spruce"
                                            )))

tmpx2<-tmpx%>%mutate(NAYTE2=parse_factor(NAYTE,levels=c("Inoculum","Extracted pine bark", "Extracted spruce bark",
                                                              "Pine bark", "Spruce bark", 
                                                              "Separated pyro pine", "Separated pyro spruce",
                                                              "Centrifuged pyro pine", "Centrifuged pyro spruce")))%>%
  filter(NAYTE2=="Inoculum" | NAYTE2=="Extracted pine bark" | NAYTE2=="Pine bark" | NAYTE2=="Spruce bark" | NAYTE2=="Extracted spruce bark")

ggplot(df, aes(day, group=day))+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity")+
  labs(title="Predicted daily methane production", x="Day", y="Methane production (unscaled)")+
#  geom_line(data=tmpx,aes(x=day, y=arvo,color=pullo))+
  geom_line(data=tmpx2,aes(x=day, y=arvo, color=pullo))+
  scale_color_manual(values=rep("red",26))+
  facet_wrap(~NAYTE2, scales="free")+
  theme_bw()+
  theme(legend.position="none", text=element_text(size=15))





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
  geom_line(data=tmpx,aes(x=day, y=arvo,color=pullo))+
  facet_wrap(~NAYTE, scales="free")+
  theme_bw()+
  theme(legend.position="none") # removes legend



#KUMULATIIVISET KERTYMÄT
df0<-read_xlsx("data/BMP_InnoTrea_Hennille.xlsx", range="C5:AC46", col_names = F)
source("data-BMP.r")

#YMPPI; skaalaamaton
df<-boxplot.jags.df(chains, "cum_y_new",c(1:n_days))
df<-setNames(df,c("day","q5","q25","q50","q75","q95"))

ggplot(df, aes(day, group=day))+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity")+
  labs(title="YMPPI, kumulatiivinen kertymä, ennustejakaumat", x="Päivä", y="Kumulatiivinen kaasumäärä")+
  geom_line(data=tmpy,aes(x=day, y=arvo,color=pullo))+
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
  geom_line(data=tmpx,aes(x=day, y=arvo,color=pullo))+
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
  facet_wrap(~NAYTE, scales="free")+
  theme_bw()+
  theme(legend.position="none") # removes legend

filter(df2, day==42)
# laske tn:t pareittain kumpi on suurempi!

for(i in 1:n_types){
  df<-boxplot.jags.df2(chains, "tulos[",str_c(i,"]"),1:n_days)%>%
    mutate(NAYTE=ExpName[i])
  ifelse(i>1, df2<-bind_rows(df2,df),df2<-df)
}

p1<-c();p2<-c();p3<-c();p4<-c()
p1<-ifelse(chains[,"tulos[42,1]"][[1]]-chains[,"tulos[42,3]"][[1]]>0,1,0) # P(Mänty KU > Mänty K) = 1
p2<-ifelse(chains[,"tulos[42,2]"][[1]]-chains[,"tulos[42,4]"][[1]]>0,1,0) # P(Kuusi KU > Kuusi K) = 0.58
p3<-ifelse(chains[,"tulos[42,5]"][[1]]-chains[,"tulos[42,7]"][[1]]>0,1,0) # P(Mänty PS > Mänty PF) = 0.19
p4<-ifelse(chains[,"tulos[42,6]"][[1]]-chains[,"tulos[42,8]"][[1]]>0,1,0) # P(Kuusi PS > Kuusi PF) = 0.43
mean(p1);mean(p2);mean(p3);mean(p4)

summary(chains[,"tulos[42,1]"], quantiles=c(0.05,0.5,0.95))
summary(chains[,"tulos[42,2]"], quantiles=c(0.05,0.5,0.95))
summary(chains[,"tulos[42,3]"], quantiles=c(0.05,0.5,0.95))
summary(chains[,"tulos[42,4]"], quantiles=c(0.05,0.5,0.95))
summary(chains[,"tulos[42,5]"], quantiles=c(0.05,0.5,0.95))
summary(chains[,"tulos[42,6]"], quantiles=c(0.05,0.5,0.95))
summary(chains[,"tulos[42,7]"], quantiles=c(0.05,0.5,0.95))
summary(chains[,"tulos[42,8]"], quantiles=c(0.05,0.5,0.95))
summary(chains[,"tulos_ymppi[42]"], quantiles=c(0.05,0.5,0.95))


summary(chains[,"tulos[42,1]"][[1]]-chains[,"tulos[42,3]"][[1]], quantiles=c(0.05,0.5,0.95))
summary(chains[,"tulos[42,2]"][[1]]-chains[,"tulos[42,4]"][[1]], quantiles=c(0.05,0.5,0.95))
summary(chains[,"tulos[42,5]"][[1]]-chains[,"tulos[42,7]"][[1]], quantiles=c(0.05,0.5,0.95))
summary(chains[,"tulos[42,6]"][[1]]-chains[,"tulos[42,8]"][[1]], quantiles=c(0.05,0.5,0.95))
