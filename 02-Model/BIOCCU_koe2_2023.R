source("01-Data/data-BMP-2023.r")


M1<-"
model{
# Perusmalli, jokainen pullosarja mallinnetaan toisistaan riippumattomina

  for(i in 1:n_days_y[1]){ # number of days per experiment 
    for(j in 1:nb_y[1]){ # number of bottles per experiment
      y1[i,j]~dlnorm(My1[i],Ty1[i]) # y: inoculum
    }
    My1[i]<-log(muy1[i])-0.5/Ty1[i]
    muy1[i]~dlnorm(log(100)-0.5*log(pow(2,2)+1),1/log(pow(2,2)+1))
    Ty1[i]<-1/log(cv_y1[i]*cv_y1[i]+1)
    cv_y1[i]~dbeta(ay[1],by[1]) # Samat hier parametrit yli kokeiden!
    y_new1[i]~dlnorm(My1[i],Ty1[i])
  }
  cum_y_new1[1]<-y_new1[1]
  for(i in 2:n_days_y[1]){
    cum_y_new1[i]<-cum_y_new1[i-1]+y_new1[i]
  }
      
  for(i in 1:n_days_y[2]){ # number of days per experiment 
    for(j in 1:nb_y[2]){ # number of bottles per experiment
      y2[i,j]~dlnorm(My2[i],Ty2[i]) # y: inoculum
    }
    My2[i]<-log(muy2[i])-0.5/Ty2[i]
    muy2[i]~dlnorm(log(100)-0.5*log(pow(2,2)+1),1/log(pow(2,2)+1))
    Ty2[i]<-1/log(cv_y2[i]*cv_y2[i]+1)
    cv_y2[i]~dbeta(ay[2],by[2]) # Samat hier parametrit yli kokeiden!
    y_new2[i]~dlnorm(My2[i],Ty2[i])
  }
  cum_y_new2[1]<-y_new2[1]
  for(i in 2:n_days_y[2]){
    cum_y_new2[i]<-cum_y_new2[i-1]+y_new2[i]
  }
  
  for(i in 1:26){ # number of days per experiment 
    for(j in 1:nb_y[3]){ # number of bottles per experiment
      y3[i,j]~dlnorm(My3[i],Ty3[i]) # y: inoculum
    }
    My3[i]<-log(muy3[i])-0.5/Ty3[i]
    muy3[i]~dlnorm(log(100)-0.5*log(pow(2,2)+1),1/log(pow(2,2)+1))
    Ty3[i]<-1/log(cv_y3[i]*cv_y3[i]+1)
    cv_y3[i]~dbeta(ay[3],by[3]) # Samat hier parametrit yli kokeiden!
    y_new3[i]~dlnorm(My3[i],Ty3[i])
  }
  cum_y_new3[1]<-y_new3[1]
  for(i in 2:26){
    cum_y_new3[i]<-cum_y_new3[i-1]+y_new3[i]
  }

  for(k in 1:3){# kokeet
    cv_y_pred[k]~dbeta(ay[k],by[k])
    ay[k]<-psiy[k]*etay[k]
    by[k]<-(1-psiy[k])*etay[k]
    psiy[k]~dunif(0.01,0.5)
    etay[k]~dunif(0.01,1000)
  }
  #cv_pred~dbeta(ay,by)
  #ay<-psiy*etay
  #by<-(1-psiy)*etay
  #psiy~dunif(0.01,0.5)
  #etay~dunif(0.01,1000)
  
  for(i in 1:n_days_x){ # number of days
    for(j in 1:4){ # number of bottles
      x1[i,j]~dlnorm(Mx[i,1],Tx[i,1]) 
      x3[i,j]~dlnorm(Mx[i,3],Tx[i,3]) 
      x4[i,j]~dlnorm(Mx[i,4],Tx[i,4])
    }
    for(j in 1:5){ # number of bottles
      x2[i,j]~dlnorm(Mx[i,2],Tx[i,2]) 
    }
  }
  for(k in 1:n_types[2]){# number of sample types
    for(i in 1:n_days_x){ # number of days
      Mx[i,k]<-log(mux[i,k])-0.5/Tx[i,k]
      mux[i,k]~dlnorm(log(100)-0.5*log(pow(2,2)+1),1/log(pow(2,2)+1))

      x_new[i,k]~dlnorm(Mx[i,k],Tx[i,k])
      Tx[i,k]<-1/log(cv_x[i,k]*cv_x[i,k]+1)
      cv_x[i,k]~dbeta(ax[k],bx[k])

      #erotus[i,k]<-cum_x_new[i,k]-cum_y_new2[i]
      #tulos[i,k]<-erotus[i,k]/const_x[k]
    }
    cum_x_new[1,k]<-x_new[1,k]
    for(i in 2:n_days_x){
      cum_x_new[i,k]<-cum_x_new[i-1,k]+x_new[i,k]
    }
    tulos[k]<-(cum_x_new[n_days_x,k]-cum_y_new2[n_days_y[2]])/const_x[k]
    
    ax[k]<-psix[k]*etax[k]
    bx[k]<-(1-psix[k])*etax[k]
    psix[k]~dunif(0.01,0.5)
    etax[k]~dunif(0.01,1000)
    cv_x_pred[k]~dbeta(ax[k],bx[k])
  }
  
}"
Mname<-"perus_koe2_maxday10_const6"
cat(M1,file=str_c(Mname,".txt"))

M2<-"
model{
# Tähän kahden tai useamman kokeen ymppipullot, oletetaan cv:t tulemaan hierarkkisesti
# samasta jakaumasta jolloin saadaan enemmän mukaan päiviä jolloin tuotanto on korkeaa
# -> vaikuttaa hierarkkiaan niin että myös pienempiä cv:itä mahdollista estimoitua
# Huomioitava kuitenkin että kunkin kokeen näytteiden tuotannosta vähennetään oikeiden 
# ymppi-pullojen tuotanto! Myös mahdollista että kaikki näytteet käsitellään saman hier cv:n kautta,
# se olisi varmaan systemaattisin tapa toimia


  for(i in 1:n_days_y[1]){ # number of days per experiment 
    for(j in 1:nb_y[1]){ # number of bottles per experiment
      y1[i,j]~dlnorm(My1[i],Ty1[i]) # y: inoculum
    }
    My1[i]<-log(muy1[i])-0.5/Ty1[i]
    muy1[i]~dlnorm(log(100)-0.5*log(pow(2,2)+1),1/log(pow(2,2)+1))
    Ty1[i]<-1/log(cv_y1[i]*cv_y1[i]+1)
    cv_y1[i]~dbeta(ay[1],by[1]) # Samat hier parametrit yli kokeiden!
    y_new1[i]~dlnorm(My1[i],Ty1[i])
  }
  cum_y_new1[1]<-y_new1[1]
  for(i in 2:n_days_y[1]){
    cum_y_new1[i]<-cum_y_new1[i-1]+y_new1[i]
  }
      
  for(i in 1:n_days_y[2]){ # number of days per experiment 
    for(j in 1:nb_y[2]){ # number of bottles per experiment
      y2[i,j]~dlnorm(My2[i],Ty2[i]) # y: inoculum
    }
    My2[i]<-log(muy2[i])-0.5/Ty2[i]
    muy2[i]~dlnorm(log(100)-0.5*log(pow(2,2)+1),1/log(pow(2,2)+1))
    Ty2[i]<-1/log(cv_y2[i]*cv_y2[i]+1)
    cv_y2[i]~dbeta(ay[2],by[2]) # Samat hier parametrit yli kokeiden!
    y_new2[i]~dlnorm(My2[i],Ty2[i])
  }
  cum_y_new2[1]<-y_new2[1]
  for(i in 2:n_days_x){
    cum_y_new2[i]<-cum_y_new2[i-1]+y_new2[i]
  }
  
  for(i in 1:26){ # number of days per experiment 
    for(j in 1:nb_y[3]){ # number of bottles per experiment
      y3[i,j]~dlnorm(My3[i],Ty3[i]) # y: inoculum
    }
    My3[i]<-log(muy3[i])-0.5/Ty3[i]
    muy3[i]~dlnorm(log(100)-0.5*log(pow(2,2)+1),1/log(pow(2,2)+1))
    Ty3[i]<-1/log(cv_y3[i]*cv_y3[i]+1)
    cv_y3[i]~dbeta(ay[3],by[3]) # Samat hier parametrit yli kokeiden!
    y_new3[i]~dlnorm(My3[i],Ty3[i])
  }
  cum_y_new3[1]<-y_new3[1]
  for(i in 2:26){
    cum_y_new3[i]<-cum_y_new3[i-1]+y_new3[i]
  }

  for(k in 1:3){# kokeet
    cv_y_pred[k]~dbeta(ay[k],by[k])
    #ay[k]<-psiy[k]*etay[k]
    #by[k]<-(1-psiy[k])*etay[k]
    ay[k]<-psiy*etay
    by[k]<-(1-psiy)*etay
    #psiy[k]~dunif(0.01,0.5)
    #etay[k]~dunif(0.01,1000)
  }
  psiy~dunif(0.01,0.5)
  etay~dunif(0.01,1000)
  
  for(i in 1:n_days_x){ # number of days
    for(j in 1:4){ # number of bottles
      x1[i,j]~dlnorm(Mx[i,1],Tx[i,1]) 
      x3[i,j]~dlnorm(Mx[i,3],Tx[i,3]) 
      x4[i,j]~dlnorm(Mx[i,4],Tx[i,4])
    }
    for(j in 1:5){ # number of bottles
      x2[i,j]~dlnorm(Mx[i,2],Tx[i,2]) 
    }
  }
  for(k in 1:n_types[2]){# number of sample types
    for(i in 1:n_days_x){ # number of days
      Mx[i,k]<-log(mux[i,k])-0.5/Tx[i,k]
      mux[i,k]~dlnorm(log(100)-0.5*log(pow(2,2)+1),1/log(pow(2,2)+1))

      x_new[i,k]~dlnorm(Mx[i,k],Tx[i,k])
      Tx[i,k]<-1/log(cv_x[i,k]*cv_x[i,k]+1)
      cv_x[i,k]~dbeta(ax[k],bx[k])

      erotus[i,k]<-cum_x_new[i,k]-cum_y_new2[i]
      tulos[i,k]<-erotus[i,k]/const_x[k]
    }
    cum_x_new[1,k]<-x_new[1,k]
    for(i in 2:n_days_x){
      cum_x_new[i,k]<-cum_x_new[i-1,k]+x_new[i,k]
    }
    ax[k]<-psix*etax
    bx[k]<-(1-psix)*etax
    #ax[k]<-psix[k]*etax[k]
    #bx[k]<-(1-psix[k])*etax[k]
    #psix[k]~dunif(0.01,0.5)
    #etax[k]~dunif(0.01,1000)
    cv_x_pred[k]~dbeta(ax[k],bx[k])
  }
  psix~dunif(0.01,0.5)
  etax~dunif(0.01,1000)
  
}"
Mname<-"commonpsi&eta_koe2"
cat(M2,file=str_c(Mname,".txt"))


data<-list(
 # nb=c(4,5,4,4),
  n_days_y=n_days_y,
  n_days_x=n_days_x[2],
  nb_y=nb_y,
  n_types=n_types,
   #const_x=rep(24.29,n_types[2]),
  const_x=rep(6.07,n_types[2]),
  x1=df_x2[[1]],x2=df_x2[[2]],x3=df_x2[[3]],
   x4=df_x2[[4]],
   y1=as.matrix(df_y1), y2=as.matrix(df_y2), y3=as.matrix(df_y3))


var_names<-c(
  "cv_y_pred",  "cv_x_pred",
  "cum_x_new", 
  "cum_y_new1","cum_y_new2","cum_y_new3",
   #"erotus",
  "tulos",
   "x_new", 
   "mux","muy1","muy2","muy3",
   "psix", "etax",
   "psiy", "etay",
   "y_new1","y_new2","y_new3"
  )


#nb of samples = samples * thin, burnin doesn't take into account thin
# sample on tässä lopullinen sample, toisin kuin rjagsissa!!!

run1 <- run.jags(M1, 
                 monitor= var_names,data=data,#inits = inits,
                 n.chains = 2, method = 'parallel', thin=20, burnin =1000, 
                 modules = "mix",keep.jags.files=F,sample =4000, adapt = 100, 
                 progress.bar=TRUE)
run<-run1
save(run, file=str_c(pathMain,"01-Projects/BMP/output/",Mname,".RData"))
summary(run)

#runall<-run1
#run<-runall
summary(run, var="eta")
summary(run, var="psi")
#summary(run, var="cv")

plot(run, var="eta")
plot(run, var="psi")


write.xlsx(summary(run, var="new"), "ennusteet.xlsx")

chains<-as.mcmc.list(run1)

#plot(run1)

