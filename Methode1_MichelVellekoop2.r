# Kalman filter for SOD data
#

#first remove all variables

rm(list=ls())

ROOTDIR <- "C:/R-programms/Bastiaan_projects/KalmanFilteringSOD/"

library (fBasics)
library (fOptions)

# underlying process parameters

mu <- 0                #
sigma <- 0.3           # vollie of stock (underlying process)
S0 <- 100              # Start value of underlying
r <- 0.04              # Risk-free interest rate
b <- mu
Time <- 1            # Time in years
Nobs <- 100
delta.t <- Time/Nobs      # time step

Pvec <- numeric(Nobs)            #
Bvec <- numeric(Nobs)            #
Stock <- numeric(Nobs)           # Stock process
Returns <- numeric(Nobs)         # day returns
Y <- numeric(Nobs)               # observed process
Z <- numeric(Nobs)               # observed process without error

# underlying process parameters


print ("def of process..")

# measurement parameters / process



# covariances process / measurement

sigy <- 0.01    # Y is measurable quantity
sigb <- 0.2     # b is slope <- quantity you would like to determine

sigy2 <- sigy*sigy
sigb2 <- sigb*sigb

# initial guess of process parameters

mu0 <- 0
sig0 <- 0.2

Slope=0.22

betahat <- mu0
Bvec[1] <- betahat

P <- sig0
Pvec[1] <- P

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

eps <- rnorm(Nobs,mean=0,sd=1)

Stock[1] <- S0
Returns[1] <- 0
print ("start")

  Y[1] <- Stock[1]*Slope+sigy*rnorm(1,mean=0,sd=1)
  Z[1] <- Stock[1]*Slope

  for (i in 2:Nobs)
  {

   dS <- wienerPath(eps[i])
   
#   print (betahat)
   Returns[i] <- dS
   Stock[i] <- Stock[i-1]*exp(dS)

   
   Y[i] <- Stock[i]*Slope+sigy*rnorm(1,mean=0,sd=1)
   Z[i] <- Stock[i]*Slope
   
   betahat <- betahat+(P*(Y[i]-(Stock[i]*betahat))/(P+sigy2))

   P <- sigb2*2+(P*sigy2/(P+sigy2))
   Pvec[i] <- P
   Bvec[i] <- betahat
   
   
  }

#  plot (Stock,Y,type='l')
  
#  MinStock=min(StockLevel[Start:End])           # determine boundaries for the graph  -> x axis
#  MaxStock=max(StockLevel[Start:End])

  MinY <- min(Stock,Y,Z)         # determine boundaries for the graph  -> y axis
  MaxY <- max(Stock,Y,Z)

  
  
  plot (Stock,type='l',ylim=c(MinY,MaxY),)
  par(new=T)
  plot (Y,type='l',ylim=c(MinY,MaxY))
  par(new=T)
  plot (Z,type='l',ylim=c(MinY,MaxY))

#  hist(Returns,freq=TRUE,br=20)
