library(ggplot2)
source("01-Data/data-BMP-2023.r")


metallit<-read_xlsx("metallit.xlsx")
metallit

df_metals<-metallit |> pivot_longer(cols=c("Fe":"Zn"), names_to="metalli_tyyppi") |> 
  rename(Experiment=expname) |> rename(metalli_pitoisuus=value)

# Koe 2 (EXPERIMENT 1 PAPERISSA)
load(file=str_c("../../01-Projects/BMP/output/perus_koe2_maxday10_const6.RData"))
chains<-window(as.mcmc.list(run), thin=80)
length(chains[,"tulos[1]"][[1]]) #4000

#summary(run, var="tulos")
#summary(run, var="cum_x_new")

y<-array(NA, dim=c(1000,12))
y_1_0<-chains[,"cum_x_new[10,1]"][[1]] # 1_0 Ei kiinnostuksen kohteena
y[,1]<-chains[,"cum_x_new[10,2]"][[1]] # 1WP10
y[,2]<-chains[,"cum_x_new[10,3]"][[1]] # 1WP16.7
y[,3]<-chains[,"cum_x_new[10,4]"][[1]] # 1WP23.3

y1<-as.data.frame(y[,1])|> mutate(Experiment="1WP10") |> rename(methane="y[, 1]")
y2<-as.data.frame(y[,2])|> mutate(Experiment="1WP16.7")|> rename(methane="y[, 2]")
y3<-as.data.frame(y[,3])|> mutate(Experiment="1WP23.3")|> rename(methane="y[, 3]")

#summary((y[,3]-y_1_0)/y_1_0) # Kokeessa 1 tuhkalisäys huonontaa tulosta

# Koe 3 (EXPERIMENT 2 PAPERISSA)

load(file=str_c("../../01-Projects/BMP/output/eta&psicommonfory_koe3_const2.RData")) # Ymppisarjoilla hier priorit cv:lle
chains<-as.mcmc.list(run)
chains<-window(as.mcmc.list(run), thin=80)

length(chains[,"tulos[1]"][[1]]) #4000

#summary(run, var="tulos")
#summary(run, var="cum_x_new")
#y<-array(NA, dim=c(4000,12))
ExpName3<-c("2_0", "2P2.5", "2P5", "2P7.5",
            "2WP2.5","2WP5","2WP7.5",
            "2W2.5","2W5","2W7.5")

y_2_0<-chains[,"cum_x_new[14,1]"][[1]] # 2_0 Ei kiinnostuksen kohteena
y[,4]<-chains[,"cum_x_new[14,2]"][[1]] # 2P2.5
y[,5]<-chains[,"cum_x_new[14,3]"][[1]]# 2P5
y[,6]<-chains[,"cum_x_new[14,4]"][[1]]# 2P7.5
y[,7]<-chains[,"cum_x_new[14,5]"][[1]]# 2WP2.5
y[,8]<-chains[,"cum_x_new[14,6]"][[1]]# 2WP5
y[,9]<-chains[,"cum_x_new[14,7]"][[1]]# 2WP7.5
y[,10]<-chains[,"cum_x_new[14,8]"][[1]]# 2W2.5
y[,11]<-chains[,"cum_x_new[14,9]"][[1]]# 2W5
y[,12]<-chains[,"cum_x_new[14,10]"][[1]]# 2W7.5


# Kokeessa 2 tuhkalisäys parantaa tulosta parhaimmillaan keskimäärin 12.8%, 90%PI 22.6-33.5%
summary((y[,11]-y_2_0)/y_2_0, quantiles=c(0.5,0.05,0.95))
summary(as.mcmc(y[,11])) # 2W5!!!!!!!! Huomaa että kuvissa järjestys on eri!!!! Mutta kaikkialla muualla onneksi sama
summary(as.mcmc(y_2_0))

# Jos halutaan vertailla tuloksia joista ymppi poistettu ja skaalaustekijällä jaettu
# Saadaan keskimäärin 30.7% ja 90%PI 17.1-45.8%
summary(chains[,"tulos[1]"][[1]], quantiles=c(0.5,0.05,0.95))
summary(chains[,"tulos[9]"][[1]], quantiles=c(0.5,0.05,0.95))

tulos_0<-chains[,"tulos[1]"][[1]]
tulos_2W5<-chains[,"tulos[9]"][[1]]
summary((tulos_2W5-tulos_0)/tulos_0, quantiles=c(0.5,0.05,0.95))



y4<-as.data.frame(y[,4])|> mutate(Experiment="2P2.5")|> rename(methane="y[, 4]")
y5<-as.data.frame(y[,5])|> mutate(Experiment="2P5")|> rename(methane="y[, 5]")
y6<-as.data.frame(y[,6])|> mutate(Experiment="2P7.5")|> rename(methane="y[, 6]")
y7<-as.data.frame(y[,7])|> mutate(Experiment="2WP2.5")|> rename(methane="y[, 7]")
y8<-as.data.frame(y[,8])|> mutate(Experiment="2WP5")|> rename(methane="y[, 8]")
y9<-as.data.frame(y[,9])|> mutate(Experiment="2WP7.5")|> rename(methane="y[, 9]")
y10<-as.data.frame(y[,10])|> mutate(Experiment="2W2.5")|> rename(methane="y[, 10]")
y11<-as.data.frame(y[,11])|> mutate(Experiment="2W5")|> rename(methane="y[, 11]")
y12<-as.data.frame(y[,12])|> mutate(Experiment="2W7.5")|> rename(methane="y[, 12]")

df_sample<-as_tibble(rbind(y1,y2,y3,y4,y5,y6,y7,y8,y9,y10,y11,y12))

df_metals

df<-full_join(df_sample, df_metals, relationship="many-to-many")

df2<-df |> #filter(metalli_tyyppi=="Fe"| metalli_tyyppi=="Cu") |> 
  mutate(mtype=as.factor(metalli_tyyppi))

ggplot(df2, aes(x=metalli_pitoisuus, y=methane, col=Experiment)) + 
  geom_point(alpha=0.1)+
  facet_wrap(~mtype, scales="free")+
  guides(colour = guide_legend
         (override.aes = list(alpha = 1)))+
  #ggtitle(label) # for the main title
  xlab("Metal concentration (mg/kg)" )+ # for the x axis label
  ylab("Methane production potential") # for the y axis label

round(cor(metallit[,2:10]),2)
round(cor(metallit2[,2:10]),2)




par(mfrow=c(3,3))
m1<-lm(y[1,]~metallit$Fe)
plot(metallit$Fe, y[1,], ylim=c(2000,5000), main="Fe")
abline(m1)

for(i in 2:4000){
mx<-lm(y[i,]~metallit$Fe)
points(metallit$Fe, y[i,])
abline(mx)
}


m1<-lm(y[1,]~metallit$Mn)
plot(metallit$Mn, y[1,], ylim=c(2000,5000), main="Mn")
abline(m1)

for(i in 2:4000){
  mx<-lm(y[i,]~metallit$Mn)
  points(metallit$Mn, y[i,])
  abline(mx)
}



m1<-lm(y[1,]~metallit$Cd)
plot(metallit$Cd, y[1,], ylim=c(2000,5000), main="Cd")
abline(m1)

for(i in 2:4000){
  mx<-lm(y[i,]~metallit$Cd)
  points(metallit$Cd, y[i,])
  abline(mx)
}


m1<-lm(y[1,]~metallit$Co)
plot(metallit$Co, y[1,], ylim=c(2000,5000), main="Co")
abline(m1)

for(i in 2:4000){
  mx<-lm(y[i,]~metallit$Co)
  points(metallit$Co, y[i,])
  abline(mx)
}


m1<-lm(y[1,]~metallit$Cr)
plot(metallit$Cr, y[1,], ylim=c(2000,5000), main="Cr")
abline(m1)

for(i in 2:4000){
  mx<-lm(y[i,]~metallit$Cr)
  points(metallit$Cr, y[i,])
  abline(mx)
}


m1<-lm(y[1,]~metallit$Cu)
plot(metallit$Cu, y[1,], ylim=c(2000,5000), main="Cu")
abline(m1)

for(i in 2:4000){
  mx<-lm(y[i,]~metallit$Cu)
  points(metallit$Cu, y[i,])
  abline(mx)
}


m1<-lm(y[1,]~metallit$Ni)
plot(metallit$Ni, y[1,], ylim=c(2000,5000), main="Ni")
abline(m1)

for(i in 2:4000){
  mx<-lm(y[i,]~metallit$Ni)
  points(metallit$Ni, y[i,])
  abline(mx)
}


m1<-lm(y[1,]~metallit$Pb)
plot(metallit$Pb, y[1,], ylim=c(2000,5000), main="Pb")
abline(m1)

for(i in 2:4000){
  mx<-lm(y[i,]~metallit$Pb)
  points(metallit$Pb, y[i,])
  abline(mx)
}


m1<-lm(y[1,]~metallit$Zn)
plot(metallit$Zn, y[1,], ylim=c(2000,5000), main="Zn")
abline(m1)

for(i in 2:4000){
  mx<-lm(y[i,]~metallit$Zn)
  points(metallit$Zn, y[i,])
  abline(mx)
}


################################################################################
# VERRATAAN ERI TUHKALAJEJA (4 kpl)
################################################################################

# Koe 2 (EXPERIMENT 1 PAPERISSA)
load(file=str_c("../../01-Projects/BMP/output/perus_koe2_maxday10_const6.RData"))
chains<-as.mcmc.list(run)

y<-array(NA, dim=c(4000,4))
y[,1]<-chains[,"cum_x_new[10,4]"][[1]] # 1WP23.3

# Koe 3 (EXPERIMENT 2 PAPERISSA)

load(file=str_c("../../01-Projects/BMP/output/eta&psicommonfory_koe3_const2.RData")) # Ymppisarjoilla hier priorit cv:lle
chains<-as.mcmc.list(run)

y[,2]<-chains[,"cum_x_new[14,4]"][[1]]# 2P7.5
y[,3]<-chains[,"cum_x_new[14,7]"][[1]]# 2WP7.5
y[,4]<-chains[,"cum_x_new[14,10]"][[1]]# 2W7.5

metallit2<-metallit |> filter(expname=="1WP23.3" | expname=="2P7.5" |
                              expname=="2WP7.5" |expname=="2W7.5" )
# 


par(mfrow=c(3,3))
m1<-lm(y[1,]~metallit2$Fe)
plot(metallit2$Fe, y[1,], ylim=c(2000,5000), main="Fe")
abline(m1)

for(i in 2:4000){
  mx<-lm(y[i,]~metallit2$Fe)
  points(metallit2$Fe, y[i,])
  abline(mx)
}


m1<-lm(y[1,]~metallit2$Mn)
plot(metallit2$Mn, y[1,], ylim=c(2000,5000), main="Mn")
abline(m1)

for(i in 2:4000){
  mx<-lm(y[i,]~metallit2$Mn)
  points(metallit2$Mn, y[i,])
  abline(mx)
}



m1<-lm(y[1,]~metallit2$Cd)
plot(metallit2$Cd, y[1,], ylim=c(2000,5000), main="Cd")
abline(m1)

for(i in 2:4000){
  mx<-lm(y[i,]~metallit2$Cd)
  points(metallit2$Cd, y[i,])
  abline(mx)
}


m1<-lm(y[1,]~metallit2$Co)
plot(metallit2$Co, y[1,], ylim=c(2000,5000), main="Co")
abline(m1)

for(i in 2:4000){
  mx<-lm(y[i,]~metallit2$Co)
  points(metallit2$Co, y[i,])
  abline(mx)
}


m1<-lm(y[1,]~metallit2$Cr)
plot(metallit2$Cr, y[1,], ylim=c(2000,5000), main="Cr")
abline(m1)

for(i in 2:4000){
  mx<-lm(y[i,]~metallit2$Cr)
  points(metallit2$Cr, y[i,])
  abline(mx)
}


m1<-lm(y[1,]~metallit2$Cu)
plot(metallit2$Cu, y[1,], ylim=c(2000,5000), main="Cu")
abline(m1)

for(i in 2:4000){
  mx<-lm(y[i,]~metallit2$Cu)
  points(metallit2$Cu, y[i,])
  abline(mx)
}


m1<-lm(y[1,]~metallit2$Ni)
plot(metallit2$Ni, y[1,], ylim=c(2000,5000), main="Ni")
abline(m1)

for(i in 2:4000){
  mx<-lm(y[i,]~metallit2$Ni)
  points(metallit2$Ni, y[i,])
  abline(mx)
}


m1<-lm(y[1,]~metallit2$Pb)
plot(metallit2$Pb, y[1,], ylim=c(2000,5000), main="Pb")
abline(m1)

for(i in 2:4000){
  mx<-lm(y[i,]~metallit2$Pb)
  points(metallit2$Pb, y[i,])
  abline(mx)
}


m1<-lm(y[1,]~metallit2$Zn)
plot(metallit2$Zn, y[1,], ylim=c(2000,5000), main="Zn")
abline(m1)

for(i in 2:4000){
  mx<-lm(y[i,]~metallit2$Zn)
  points(metallit2$Zn, y[i,])
  abline(mx)
}




