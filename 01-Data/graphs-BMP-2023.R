
#PÄIVITTÄISET ENNUSTEJAKAUMAT
#source("data-BMP-2023.r")

plotBMP<-function(df, dat, points, cumul,many, title, xlab, ylab){
  plot1<-ggplot(df, aes(day, group=day))+
    geom_boxplot(
      aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
      stat = "identity")+
    labs(title=title, x=xlab, y=ylab)+
    {if(points==T & cumul==T)geom_point(data=dat,aes(x=day, y=cumul,color=pullo))}+
    {if(points==T & cumul==F)geom_point(data=dat,aes(x=day, y=arvo,color=pullo))}+
    #{if(points==T & cumul==T)geom_point(data=dat,aes(x=day, y=cumul), fill=grey)}+
    #{if(points==T & cumul==F)geom_point(data=dat,aes(x=day, y=arvo), fill=grey)}+
    {if(many==T)facet_wrap(~NAYTE)}+
    #scale_colour_grey()+
    #coord_cartesian(ylim=c(0,50))+
    #theme_bw()+
    theme(legend.position="none") # removes legend  
  plot1
}

make_df_nayte<-function(chains,n_types,n_days,name,expname){
  for(i in 1:n_types){
    df<-boxplot.jags.df2(chains, str_c(name,"["),str_c(i,"]"),1:n_days)%>%
        mutate(NAYTE=expname[i])
    ifelse(i>1, df2<-bind_rows(df2,df),df2<-df)
  }
  df2<-setNames(df2,c("day","ka","q5","q25","q50","q75","q95","NAYTE"))
  df2
}



# Koe 1
# ==============
#load(file=str_c(pathMain,"01-Projects/BMP/output/perus_koe1.RData")) # Kaikki sarjat riippumattomia toisistaan
#load(file=str_c(pathMain,"01-Projects/BMP/output/perus_koe1_const1.RData")) # Kaikki sarjat riippumattomia toisistaan
load(file=str_c("../../01-Projects/BMP/output/perus_koe1_const12.RData")) # Kaikki sarjat riippumattomia toisistaan
#load(file=str_c(pathMain,"01-Projects/BMP/output/commonpsi&eta_koe1.RData"))
chains<-as.mcmc.list(run)

# YMPPI
# ==============

  df<-boxplot.jags.df(chains, "y_new1",c(1:n_days_y[1]))
  df<-setNames(df,c("day","q5","q25","q50","q75","q95"))
  plotBMP(df,tmpy1,points=T,cumul=F,many=F,"Ymppi, päivittäisen lisäyksen ennustejakaumat, koe 1", "Päivä", "Skaalaamaton kaasumäärä")
  
  # skaalaamaton, kumulatiivinen
  df<-boxplot.jags.df(chains, "cum_y_new1",c(1:n_days_y[1]))
  df<-setNames(df,c("day","q5","q25","q50","q75","q95"))
  plotBMP(df,tmpy1,points=T,cumul=T,many=F,"Ymppi, kumulatiivinen kertymä, ennustejakaumat, koe 1", "Päivä", "Kumulatiivinen kaasumäärä")

# NÄYTTEET
# ==============
  
  df<-make_df_nayte(chains,n_types[1], n_days_x[1], "x_new", expname= ExpName1)
  plotBMP(df,dat1,points=T,cumul=F,many=T,"NÄYTTEET, päivittäisen lisäyksen ennustejakaumat, koe 1", "Päivä", "Skaalaamaton kaasumäärä")
  
  # kumulatiiviset
  df<-make_df_nayte(chains,n_types[1], n_days_x[1], "cum_x_new", expname= ExpName1)
  #plotBMP(df,dat1,points=T,cumul=T,many=T,"NÄYTTEET, kumulatiivinen kertymä, ennustejakaumat, koe 1", "Päivä", "Kumulatiivinen kaasumäärä")
  plotBMP(df,dat1,points=T,cumul=T,many=T,"Experiment 1, predicted cumulative methane production ", "Day", "Cumulative production of methane")
  
# SKAALATUT EROTUKSET
# ==============
  
  df<-make_df_nayte(chains,n_types[1], n_days_x[1], "tulos", expname= ExpName1)
  plotBMP(df,dat1,points=F,cumul=T,many=T,"Skaalattu erotus, kumulatiivinen, koe 1", "Päivä", "Kumulatiivinen skaalattu erotus")

  filter(df, day==33)
#summary(chains[, "tulos[33,1]"], quantiles = c(0.05,0.5,0.95))
  
  samp<-array(NA, dim=c(9,length(chains[, "y_new1[1]"][[1]])))
  prob<-c()
  for(i in 2:9){
    samp[i,]<-ifelse(chains[,str_c("tulos[33,",i,"]")][[1]]-chains[,"tulos[33,1]"][[1]]>0,1,0) 
    
    prob[i]<-mean(samp[i,])
  }
  prob
  

  
  
##############################################################################
# Koe 2
#load(file=str_c(pathMain,"01-Projects/BMP/output/perus_koe2.RData")) # Kaikki sarjat riippumattomia toisistaan
#load(file=str_c(pathMain,"01-Projects/BMP/output/commonpsi&eta_koe2.RData")) # Ymppisarjoilla ja näytesarjoilla omat hier priorit cv:lle
#load(file=str_c(pathMain,"01-Projects/BMP/output/perus_koe2_maxday10.RData"))
load(file=str_c("../../01-Projects/BMP/output/perus_koe2_maxday10_const6.RData"))
chains<-as.mcmc.list(run)


# YMPPI
# ==============
  df<-boxplot.jags.df(chains, "y_new2",c(1:n_days_y[2]))
  df<-setNames(df,c("day","q5","q25","q50","q75","q95"))
  plotBMP(df,tmpy2,points=T,cumul=F,many=F,"Ymppi, päivittäisen lisäyksen ennustejakaumat, koe 2", "Päivä", "Skaalaamaton kaasumäärä")
  
  # skaalaamaton, kumulatiivinen
  df<-boxplot.jags.df(chains, "cum_y_new2",c(1:n_days_y[2]))
  df<-setNames(df,c("day","q5","q25","q50","q75","q95"))
  plotBMP(df,tmpy2,points=T,cumul=T,many=F,"Ymppi, kumulatiivinen kertymä, ennustejakaumat, koe 2", "Päivä", "Kumulatiivinen kaasumäärä")

# NÄYTTEET
# ==============
  df<-make_df_nayte(chains,n_types[2], n_days_x[2], "x_new", expname= ExpName2)
  plotBMP(df,dat2,points=T,cumul=F,many=T,"NÄYTTEET, päivittäisen lisäyksen ennustejakaumat, koe 2", "Päivä", "Skaalaamaton kaasumäärä")
  
  # kumulatiiviset HUOM! YLÄOTSIKKO MUUTETTU -> EXPERIMENT 1
  df<-make_df_nayte(chains,n_types[2], n_days_x[2], "cum_x_new", expname= ExpName2)
  #plotBMP(df,dat2,points=T,cumul=T,many=T,"Experiment 2, predicted cumulative methane production ", "Day", "Cumulative production of methane")
  plotBMP(df,dat2,points=T,cumul=T,many=T,"Experiment 1, predicted cumulative methane production ", "Day", "Cumulative production of methane")
  
# SKAALATUT EROTUKSET
# ==============
  summary(run, var="tulos")
  #df<-make_df_nayte(chains,n_types[2], n_days_x[2], "tulos", expname= ExpName2)
  #plotBMP(df,dat2,points=F,cumul=T,many=T,"Skaalattu erotus, kumulatiivinen, koe 2", "Päivä", "Kumulatiivinen skaalattu erotus")

filter(df, day==10)

samp<-array(NA, dim=c(4,length(chains[, "tulos[1]"][[1]])))
prob<-c()
for(i in 2:4){
  #samp[i,]<-ifelse(chains[,str_c("tulos[28,",i,"]")][[1]]-chains[,"tulos[28,1]"][[1]]>0,1,0) 
  samp[i,]<-ifelse(chains[,str_c("tulos[",i,"]")][[1]]-chains[,"tulos[1]"][[1]]>0,1,0) 
  
  prob[i]<-mean(samp[i,])
}
prob

summary(run, var="tulos")



##############################################################################
# Koe 3
#load(file=str_c(pathMain,"01-Projects/BMP/output/perus_koe3.RData")) # Kaikki sarjat riippumattomia toisistaan
#load(file=str_c(pathMain,"01-Projects/BMP/output/eta&psicommonfory_koe3.RData")) # Ymppisarjoilla hier priorit cv:lle
load(file=str_c("../../01-Projects/BMP/output/eta&psicommonfory_koe3_const2.RData")) # Ymppisarjoilla hier priorit cv:lle
chains<-as.mcmc.list(run)


# YMPPI
# ==============
df<-boxplot.jags.df(chains, "y_new3",c(1:n_days_y[3]))
df<-setNames(df,c("day","q5","q25","q50","q75","q95"))
plotBMP(df,tmpy3,points=T,cumul=F,many=F,"Ymppi, päivittäisen lisäyksen ennustejakaumat, koe 3", "Päivä", "Skaalaamaton kaasumäärä")

# skaalaamaton, kumulatiivinen
df<-boxplot.jags.df(chains, "cum_y_new3",c(1:n_days_y[3]))
df<-setNames(df,c("day","q5","q25","q50","q75","q95"))
plotBMP(df,tmpy3,points=T,cumul=T,many=F,"Ymppi, kumulatiivinen kertymä, ennustejakaumat, koe 3", "Päivä", "Kumulatiivinen kaasumäärä")

# NÄYTTEET
# ==============
df<-make_df_nayte(chains,n_types[3], n_days_x[3], "x_new", expname= ExpName3)
plotBMP(df,dat3,points=T,cumul=F,many=T,"NÄYTTEET, päivittäisen lisäyksen ennustejakaumat, koe 3", "Päivä", "Skaalaamaton kaasumäärä")

# kumulatiiviset HUOM! YLÄOTSIKKO MUUTETTU -> EXPERIMENT 2
df<-make_df_nayte(chains,n_types[3], n_days_x[3], "cum_x_new", expname= ExpName3)
#plotBMP(df,dat3,points=T,cumul=T,many=T,"Experiment 3, predicted cumulative methane production ", "Day", "Cumulative production of methane")
plotBMP(df,dat3,points=T,cumul=T,many=T,"Experiment 2, predicted cumulative methane production ", "Day", "Cumulative production of methane")

# SKAALATUT EROTUKSET
# ==============
#df<-make_df_nayte(chains,n_types[3], n_days_x[3], "tulos", expname= ExpName3)
#plotBMP(df,dat3,points=F,cumul=T,many=T,"Skaalattu erotus, kumulatiivinen, koe 3", "Päivä", "Kumulatiivinen skaalattu erotus")
#filter(df, day==14)

summary(run, var="tulos", confidence=0.9)

# Verrataan näytteitä pelkän biojätteen tuotantoon
samp<-array(NA, dim=c(10,length(chains[, "tulos[1]"][[1]])))
prob<-c()
for(i in 2:10){
  samp[i,]<-ifelse(chains[,str_c("tulos[",i,"]")][[1]]-chains[,"tulos[1]"][[1]]>0,1,0) 
  
  prob[i]<-mean(samp[i,])
}
prob

# Verrataan puutuhkakokeita (Saa näytteet) turvetuhkakokeisiin

samp<-array(NA, dim=c(10,length(chains[, "tulos[1]"][[1]])))
prob<-array(NA, dim=c(3,3))
for(j in 1:3){
  if(j==1){vrt=chains[,"tulos[2]"][[1]]}
  if(j==2){vrt=chains[,"tulos[3]"][[1]]}
  if(j==3){vrt=chains[,"tulos[4]"][[1]]}
  for(i in 8:10){
    samp[i,]<-ifelse(chains[,str_c("tulos[",i,"]")][[1]]-vrt>0,1,0) 
    prob[i-7,j]<-mean(samp[i,])
  }
}
round(prob,2)

# Saa3 vs Turvetuhka1
mean(ifelse(chains[,"tulos[10]"][[1]]-chains[,"tulos[2]"][[1]]>0,1,0) )

par(mfrow=c(3,3))
plot(density(chains[,"tulos[8]"][[1]]), ylim=c(0,0.006),main="", xlab="")
lines(density(chains[,"tulos[2]"][[1]]), lty=2)
legend("topright",legend=c("Saa1", "Turvetuhka1"), lty=c(1,2))
plot(density(chains[,"tulos[8]"][[1]]), ylim=c(0,0.006),main="", xlab="")
lines(density(chains[,"tulos[3]"][[1]]), lty=2)
legend("topright",legend=c("Saa1", "Turvetuhka2"), lty=c(1,2))
plot(density(chains[,"tulos[8]"][[1]]), ylim=c(0,0.006),main="", xlab="")
lines(density(chains[,"tulos[4]"][[1]]), lty=2)
legend("topright",legend=c("Saa1", "Turvetuhka3"), lty=c(1,2))


plot(density(chains[,"tulos[9]"][[1]]), xlim=c(1500,2900),ylim=c(0,0.008),main="", xlab="")
lines(density(chains[,"tulos[2]"][[1]]), lty=2)
legend("topleft",legend=c("Saa2", "Turvetuhka1"), lty=c(1,2))
plot(density(chains[,"tulos[9]"][[1]]), xlim=c(1500,2900),ylim=c(0,0.008),main="", xlab="")
lines(density(chains[,"tulos[3]"][[1]]), lty=2)
legend("topleft",legend=c("Saa2", "Turvetuhka2"), lty=c(1,2))
plot(density(chains[,"tulos[9]"][[1]]), xlim=c(1500,2900),ylim=c(0,0.008),main="", xlab="")
lines(density(chains[,"tulos[4]"][[1]]), lty=2)
legend("topleft",legend=c("Saa2", "Turvetuhka3"), lty=c(1,2))

plot(density(chains[,"tulos[10]"][[1]]), xlim=c(1500,2500),main="", xlab="")
lines(density(chains[,"tulos[2]"][[1]]), lty=2)
legend("topleft",legend=c("Saa3", "Turvetuhka1"), lty=c(1,2))
plot(density(chains[,"tulos[10]"][[1]]), xlim=c(1500,2500),main="", xlab="")
lines(density(chains[,"tulos[3]"][[1]]), lty=2)
legend("topleft",legend=c("Saa3", "Turvetuhka2"), lty=c(1,2))
plot(density(chains[,"tulos[10]"][[1]]), xlim=c(1500,2500),main="", xlab="")
lines(density(chains[,"tulos[4]"][[1]]), lty=2)
legend("topleft",legend=c("Saa3", "Turvetuhka3"), lty=c(1,2))




