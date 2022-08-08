# Kalman filter for SOD data
#

#first remove all variables

rm(list=ls())

ROOTDIR<-"C:/R-programms/Bastiaan_projects/KalmanFilteringSOD/"

library (fBasics)
library (fOptions)

# underlying process parameters

mu<-0
sigma<-0.2
S0<-100
r<-0.04

b<-mu
Nobs<-10000
delta.t<-1/Nobs

Pvec<-numeric(Nobs)            #
bvec<-numeric(Nobs)            #
Stock<-numeric(Nobs)           # Stock process
Returns<-numeric(Nobs)         # day returns
Y<-numeric(Nobs)               # observed process

# underlying process parameters


print ("def of process..")

# measurement parameters / process

Nobs<-10000


# covariances process / measurement

sigy<-0.01    # Y is measurable quantity
sigb<-0.2     # b is slope <- quantity you would like to determine

sigy2<-sigy*sigy
sigb2<-sigb*sigb

# initial guess of process parameters

mu0<-0
sig0<-0.2

betahat<-mu0
bvec[1]<-betahat

P<-sig0
Pvec[1]<-P

# measurement parameters / process

print ("def of function")

wienerPath <- function(eps) {
  # Note, the option parameters must be globally defined!
  # Generate the Paths:
  path <- (b-sigma*sigma/2)*delta.t + sigma*sqrt(delta.t)*eps
  # Return Value:
  path }

RNGkind("Mersenne-Twister")#matches  "Super-Duper"
runif(1)

eps<-rnorm(Nobs,mean=0,sd=sigma)

Stock[1]<-S0
Returns[1]<-0
print ("start")


  for (i in 2:Nobs)
  {

   dS<-wienerPath(eps[i])
   
   print (dS)
   Returns[i]<-dS
   Stock[i]<-Stock[i-1]*exp(dS)
   
   
   
   
   P<-sigb2*2+(P*sigy2/(P+sigy2))
   betahat<-betahat+P*(y[i]
   
   
  }

  plot (Stock,type='l')
  hist(Returns,freq=TRUE,br=20)
