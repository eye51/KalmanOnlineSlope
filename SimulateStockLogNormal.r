# Simulate LogNormal Stock process
#

#first remove all variables

# rm(list=ls())

ROOTDIR <- "C:/R-programms/Bastiaan_projects/KalmanFilteringSOD/"

library (fBasics)
library (fOptions)

# underlying process parameters

mu <- 0                #
sigma <- 0.3           # vollie of stock (underlying process)
S0 <- 100              # Start value of underlying
X <- 100               # Strike
r <- 0.04              # Risk-free interest rate
Time <- 1              # time in years



Nobs <- 2                 # number of observations during Time
Nsimpstep <- 5000         # number of simulations per simulation step
Nsimsteps <-100            # total numer of simulations is divided in .. steps

delta.t <- 1/Nobs      # time step

Nobs <- Nobs+1

Stock <- numeric(Nobs)           # Stock process
Returns <- numeric(Nobs)         # day returns

TotReturns <- numeric(Nsimpstep*Nsimsteps)       # T returns
CallVal    <- numeric (Nsimsteps)

# underlying process parameters

wienerPath  <-  function(eps) {
  # Note, the option parameters must be globally defined!
  # Generate the Paths:
  path  <-  (mu-sigma*sigma/2)*delta.t + sigma*sqrt(delta.t)*eps
  # Return Value:
  path }


RNGkind("Mersenne-Twister")#matches  "Super-Duper"
runif(1)


print ("start")

avg_payout <- 0.0

for (k in 1:Nsimsteps)
{
    print (k)
    for (j in 1:Nsimpstep)
    {

      Stock[1] <- S0
      Returns[1] <- 0
      eps <- rnorm(Nobs,mean=0,sd=1)
      for (i in 2:Nobs)
      {

       dS <- wienerPath(eps[i])
       Returns[i] <- dS
       Stock[i] <- Stock[i-1]*exp(dS)

      } # Loop over Path

      payout <- max(0,(Stock[Nobs]-X))
      TotReturns[j]<-(Stock[Nobs]-S0)/S0
      avg_payout <- avg_payout+payout


    }  # loop over Nsimpstep
    
    CallVal[k] <- avg_payout/(Nsimpstep*k)
    print (avg_payout/(Nsimpstep*k))
}

#  plot (Stock,Y,type='l')

  plot (Stock,type='l')
#  hist(Returns,freq=TRUE,br=20)
