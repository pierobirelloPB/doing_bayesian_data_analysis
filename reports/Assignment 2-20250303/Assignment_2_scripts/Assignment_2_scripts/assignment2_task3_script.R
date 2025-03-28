rm(list = ls())
graphics.off()
source("openGraphSaveGraph.R")
require(rjags)
savePath = "Figures/ExerciseC/"

#-----------------------------------------------------------------------------
# THE MODEL.
modelString = "
  model {
    for ( cIdx in 1:Ncat ) {
      z[cIdx] ~ dbin( theta[cIdx] , N[cIdx] )
      theta[cIdx] ~ dbeta( omega*(kappa-2)+1 , 
                           (1-omega)*(kappa-2)+1 ) 
    }
    omega ~ dbeta( omega0*(kappa0-2)+1, omega0*(kappa0-2)+1 ) 
    kappa <- kappaMinusTwo + 2
    kappaMinusTwo ~ dgamma( 0.01 , 0.01 )  # mean=1 , sd=10 (generic vague)
    omega0 <- 0.3
    kappa0 <- 10
  }
  " # close quote for modelString
writeLines( modelString , con="Cmodel.txt" )

#------------------------------------------------------------------------------
# THE DATA.


z = c(8,11,14)
N = c(30,30,30)
Ncat =  length(unique(z))
# Specify the data in a list, for later shipment to JAGS:
dataList = list(
  z = z ,
  N = N ,
  Ncat = Ncat
)

#------------------------------------------------------------------------------
# INTIALIZE THE CHAIN.

# Initial values of MCMC chains based on data:
initsList = function() {
  thetaInit = rep(NA,Ncat)
  for ( cIdx in 1:Ncat ) { # for each subject
    resampledZ = rbinom(1, size=N[cIdx] , prob=z[cIdx]/N[cIdx] )
    thetaInit[cIdx] = resampledZ/N[cIdx]
  }
  thetaInit = 0.001+0.998*thetaInit # keep away from 0,1    
  kappaInit = 100 # lazy, start high and let burn-in find better value
  return( list( theta=thetaInit , 
                omegaO=mean(thetaInit) ,
                kappaMinusTwoO=kappaInit-2 ) )
}

#------------------------------------------------------------------------------
# RUN THE CHAINS.

parameters = c( "theta" )   # The parameter(s) to be monitored.
adaptSteps = 1000              # Number of steps to "tune" the samplers.
burnInSteps = 5000            # Number of steps to "burn-in" the samplers.
nChains = 3                   # Number of chains to run.
numSavedSteps=50000           # Total number of steps in chains to save.
thinSteps=1                   # Number of steps to "thin" (1=keep every step).
nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains ) # Steps per chain.
# Create, initialize, and adapt the model:
jagsModel = jags.model( "Cmodel.txt" , data=dataList , inits=initsList , 
                        n.chains=nChains , n.adapt=adaptSteps )
# Burn-in:
cat( "Burning in the MCMC chain...\n" )
update( jagsModel , n.iter=burnInSteps )
# The saved MCMC chain:
cat( "Sampling final MCMC chain...\n" )
codaSamples = coda.samples( jagsModel , variable.names=parameters , 
                            n.iter=nIter , thin=thinSteps )


#------------------------------------------------------------------------------
# EXAMINE THE RESULTS.
mcmcChain = as.matrix( codaSamples )

# Extract the posterior samples
theta1Sample = mcmcChain[, "theta[1]"]
theta2Sample = mcmcChain[, "theta[2]"]
theta3Sample = mcmcChain[, "theta[3]"]

# Compute differences:
thetaDiff12 = theta1Sample - theta2Sample
thetaDiff13 = theta1Sample - theta3Sample
thetaDiff23 = theta2Sample - theta3Sample

# Load plotting function
source("plotPost.R")

# Open a plotting window
openGraph(width=12, height=8)
par(mfrow=c(2,3)) # 2 rows, 3 columns for all 6 plots

# Plot posteriors for theta1, theta2, theta3
plotPost(theta1Sample, xlab=expression(theta[1]), main="Posterior of θ1")
plotPost(theta2Sample, xlab=expression(theta[2]), main="Posterior of θ2")
plotPost(theta3Sample, xlab=expression(theta[3]), main="Posterior of θ3")

# Plot posteriors for differences
plotPost(thetaDiff12, xlab=expression(theta[1] - theta[2]), main="θ1 - θ2",
         compVal=0.0,ROPE=c(-0.05,0.05))
plotPost(thetaDiff13, xlab=expression(theta[1] - theta[3]), main="θ1 - θ3",
         compVal=0.0,ROPE=c(-0.05,0.05))
plotPost(thetaDiff23, xlab=expression(theta[2] - theta[3]), main="θ2 - θ3",
         compVal=0.0,ROPE=c(-0.05,0.05))

# Save the graph
saveGraph(file = paste0(savePath, "ThetaPosteriors"), type="eps")
