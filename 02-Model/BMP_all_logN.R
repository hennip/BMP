source("01-Data/data-BMP-2023.r")


M1<-"
model{

  for(i in 1:n_days){ # number of days
    for(j in 1:3){ # number of bottles
      y[i,j]~dlnorm(My[i],Ty[i]) # y: YMPPI
    }
 
  My[i]<-log(muy[i])-0.5/Ty[i]
  muy[i]~dlnorm(log(100)-0.5*log(pow(2,2)+1),1/log(pow(2,2)+1))
  
  Ty[i]<-1/log(cv_y[i]*cv_y[i]+1)
  cv_y[i]~dbeta(ay,by)
  y_new[i]~dlnorm(My[i],Ty[i])
  }
  cum_y_new[1]<-y_new[1]
  for(i in 2:n_days){
    cum_y_new[i]<-cum_y_new[i-1]+y_new[i]
  }
  ay<-psiy*etay
  by<-(1-psiy)*etay
  psiy~dunif(0.01,0.5)
  etay~dunif(0.01,1000)

  
  for(i in 1:n_days){ # number of days
    for(j in 1:3){ # number of bottles
      x1[i,j]~dlnorm(Mx[i,1],Tx[i,1]) # x1: NÄYTE uutettu kuori mänty
      x2[i,j]~dlnorm(Mx[i,2],Tx[i,2]) # x2: NÄYTE uutettu kuori kuusi
      x3[i,j]~dlnorm(Mx[i,3],Tx[i,3]) # x3: NÄYTE kuori mänty
      x4[i,j]~dlnorm(Mx[i,4],Tx[i,4]) # x4: NÄYTE kuori kuusi
      x5[i,j]~dlnorm(Mx[i,5],Tx[i,5]) # x5: NÄYTE pyro separoitu mänty
      x6[i,j]~dlnorm(Mx[i,6],Tx[i,6]) # x6: NÄYTE pyro separoitu kuusi
      x7[i,j]~dlnorm(Mx[i,7],Tx[i,7]) # x7: NÄYTE pyro fuugattu mänty
      x8[i,j]~dlnorm(Mx[i,8],Tx[i,8]) # x8: NÄYTE pyro fuugattu kuusi
    }
  }
  for(k in 1:n_types){# number of sample types
    for(i in 1:n_days){ # number of days
      Mx[i,k]<-log(mux[i,k])-0.5/Tx[i,k]
      mux[i,k]~dlnorm(log(100)-0.5*log(pow(2,2)+1),1/log(pow(2,2)+1))

      x_new[i,k]~dlnorm(Mx[i,k],Tx[i,k])
      Tx[i,k]<-1/log(cv_x[i,k]*cv_x[i,k]+1)
      cv_x[i,k]~dbeta(ax[k],bx[k])

      erotus[i,k]<-cum_x_new[i,k]-cum_y_new[i]
      tulos[i,k]<-erotus[i,k]/const_x[k]
    }
    cum_x_new[1,k]<-x_new[1,k]
    for(i in 2:n_days){
      cum_x_new[i,k]<-cum_x_new[i-1,k]+x_new[i,k]
    }
    ax[k]<-psix[k]*etax[k]
    bx[k]<-(1-psix[k])*etax[k]
    psix[k]~dunif(0.01,0.5)
    etax[k]~dunif(0.01,1000)
  }
  for(i in 1:n_days){ # number of days
    tulos_ymppi[i]<-cum_y_new[i]/const_y
  }
}"

modelName<-"test"

Mname<-str_c(modelName, ".txt")
cat(M1,file=Mname)

data<-list(
  n_days=n_days,
  n_types=n_types,
  const_x=c(rep(7.46,4),0.18,0.13,0.17,0.14),
  const_y=14.91,
  x1=df_x1,x2=df_x2,x3=df_x3,
  x4=df_x4,x5=df_x5,x6=df_x6,
  x7=df_x7,x8=df_x8,
  y=as.matrix(df_y))

var_names<-c(
  "cum_x_new", "cum_y_new",
  "erotus","tulos","tulos_ymppi",
  "x_new", "cv_x",
  "mux","muy",
  "psix", "etax",
  "psiy", "etay",
  "y_new", "cv_y")


#nb of samples = samples * thin, burnin doesn't take into account thin
# sample on tässä lopullinen sample, toisin kuin rjagsissa!!!

run1 <- run.jags(M1, 
                 monitor= var_names,data=data,#inits = inits,
                 n.chains = 2, method = 'parallel', thin=10, burnin =1000, 
                 modules = "mix",keep.jags.files=T,sample =2000, adapt = 100, 
                 progress.bar=TRUE)
#run<-run1
summary(run1, var="eta")
summary(run1, var="psi")
summary(run1, var="cv")

plot(run1, var="eta")
plot(run1, var="psi")


write.xlsx(summary(run1, var="new"), "ennusteet.xlsx")

chains<-as.mcmc.list(run1)

#plot(run1)

