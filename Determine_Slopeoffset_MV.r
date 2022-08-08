# Kalman filter for SOD data
#
#first remove all variables & load needed libraries & initialize RNG

rm(list=ls())

ROOTDIR <- "C:/R-programms/Bastiaan_projects/KalmanFilteringSOD/"

library (fBasics)
library (fOptions)

RNGkind("Mersenne-Twister")#matches  "Super-Duper"

# definition of used functions

wienerPath <- function(eps) {
  # Note, the option parameters must be globally defined!
  # Generate the Paths:
  path <- (r-sigma*sigma/2)*delta.t + sigma*sqrt(delta.t)*eps
  # Return Value:
  path }


# Start of MAIN

# underlying process parameters

Nobs <- 1000                     # number of observations in Time
Pavec <- numeric(Nobs)           # Kalman Gain <- offset
Pbvec <- numeric(Nobs)           # Kalman Gain <- slope
Pabvec <- numeric(Nobs)          # Kalman Gain <- crossterm offset<>slope
Bvec <- numeric(Nobs)            # estimation of slope
Avec <- numeric(Nobs)            # estimation of offset
Stock <- numeric(Nobs)           # Stock process
Returns <- numeric(Nobs)         # day returns
Diff <- numeric(Nobs)            #
psigy <- numeric(Nobs)           #
Y <- numeric(Nobs)               # observed process
Z <- numeric(Nobs)               # observed process without error

mu <- 0                          #
sigma <- 0.3                     # vollie of stock (underlying process)
S0 <- 1                          # Start value of underlying
Stock[1] <- S0
Returns[1] <- 0
r <- 0.04                        # Risk-free interest rate
b <- mu
Time <- 1                        # Time in years
delta.t <- Time/Nobs             # time step between each observation


# end underlying process parameters

print ("def of process..")

# covariances process / measurement

sigy <- 0.01    # Y is measurable quantity
sigb <- 0.1     # b is slope <- quantity you would like to determine

sigy2 <- sigy*sigy
sigb2 <- sigb*sigb

# initial guess of process parameters

mu0 <- 0
sig0 <- 0.2
Slope <- 0.22
Offset <- 0.40

betahat <- mu0             # initial value (guess?) for slope
Bvec[1] <- betahat
Avec[1] <- 0.0
P <- sig0                  # initial value (guess?) for Kalman Gain
Pavec[1] <- P


# measurement parameters / process

print ("def of function")

eps <- rnorm(Nobs,mean=0,sd=1)    # random variables used for Stock Process

print ("start")

  Y[1] <- Stock[1]*Slope+Offset+sigy*rnorm(1,mean=0,sd=1)
  Z[1] <- Stock[1]*Slope+Offset

  for (i in 2:Nobs)
  {

   dS <- wienerPath(eps[i])

#   print (betahat)
   Returns[i] <- dS
   Stock[i] <- Stock[i-1]*exp(dS)


   if (i < 200)
   {
       Slope=0.22
   }
   else if (i>700)
   {
        Slope=0.45
        Offset=0.30
   }
   else
   {

       Slope=0.65

   }


   Y[i] <- Stock[i]*Slope+Offset+sigy*rnorm(1,mean=0,sd=1)
   Z[i] <- Stock[i]*Slope

   P <- sigb2 +((P*sigy2)/(P+sigy2)) # in code Michel sigb2*2, in Memo: sigb2

   Diff[i] <- (Y[i]-Stock[i]*betahat)

   betahat <- betahat+P*((Y[i]-(Stock[i]*betahat))/(P+sigy2))

   psigy[i] <-(P+sigy2)

   print (betahat)
   Pavec[i] <- P
   Bvec[i] <- betahat


  }

#  plot (Stock,Y,type='l')

#  MinStock=min(StockLevel[Start:End])           # determine boundaries for the graph  -> x axis
#  MaxStock=max(StockLevel[Start:End])

  MinY <- min(Stock,Y,Z)         # determine boundaries for the graph  -> y axis
  MaxY <- max(Stock,Y,Z)

  MinYZ <- min(Y,Z)
  MaxYZ <- max(Y,Z)

#  FilenameGraph<-paste(ROOTDIR,"KalmanFit_",day,".bmp",sep="")

#  bmp(filename = FilenameGraph, width = 1020, height = 1204,res=NA)

  par (mfcol=c(3,1))          # set 4 graphs under eachother

# graph 1

  plot (Stock,type='l',ylim=c(MinY,MaxY),ylab="S / Y")
  par(new=T)
  plot (Y,type='l',ylim=c(MinY,MaxY),ylab="S / Y")
  par(new=T)
  plot (Z,type='l',ylim=c(MinY,MaxY),ylab="S / Y")

# graph 2

  MinY <- min(Bvec)         # determine boundaries for the graph  -> y axis
  MaxY <- max(Bvec)


  plot (Bvec,type='l',ylim=c(MinY,MaxY),ylab="betahat")

# graph 3

  MinY <- min(Diff)         # determine boundaries for the graph  -> y axis
  MaxY <- max(Diff)


  plot (Diff,type='l',ylim=c(MinY,MaxY),ylab="Diff")


# END of MAIN



