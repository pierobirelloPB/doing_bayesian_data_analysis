rm(list = ls())
graphics.off()
source("openGraphSaveGraph.R")
require(rjags)
savePath = "Figures/ExerciseB/"
# Kruschke, J. K. (2011). Doing Bayesian Data Analysis:
# A Tutorial with R and BUGS. Academic Press / Elsevier.
#------------------------------------------------------------------------------
# THE MODEL.

# Specify the model in JAGS language, but save it as a string in R:
modelString = "
# JAGS model specification
model {
# Likelyhood
for (j in 1:nItemsTotal) {
y[j] ~ dbin( theta[machines[j]], 40 )
}
# Prior
for (i in 1:nMachines){
theta[i] ~ dnorm( normMu, 1/normSigma^2 )T( 0.0,0.5 )
}
# Hyperpriors
normMu ~ dbeta( 1.1, 9.9 )
normSigma ~ dgamma( 2, 28 )
}
# ... JAGS model specification ends.
" 
# close quote to end modelString

# Write the modelString to a file, using R commands:
writeLines(modelString,con="model.txt")

#------------------------------------------------------------------------------
# THE DATA.


dataList = list(
y=c(1, 5, 5, 5, 2, 4, 2, 1, 3, 3, 3, 5, 3, 4, 6, 4, 2, 4, 5, 1, 1, 3, 2, 4, 1),
machines = rep(c(1,2,3,4,5),each=5),
nMachines = 5,
#nItems=rep(40, 25),     
nItemsTotal = 25
)

#------------------------------------------------------------------------------
# INTIALIZE THE CHAIN.

#------------------------------------------------------------------------------
# RUN THE CHAINS.

parameters = c( "normMu" , "normSigma" , "theta" )   # The parameter(s) to be monitored.
adaptSteps = 1000              # Number of steps to "tune" the samplers.
burnInSteps = 5000            # Number of steps to "burn-in" the samplers.
nChains = 3                   # Number of chains to run.
numSavedSteps=50000           # Total number of steps in chains to save.
thinSteps=1                   # Number of steps to "thin" (1=keep every step).
nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains ) # Steps per chain.
# Create, initialize, and adapt the model:
jagsModel = jags.model( "model.txt" , data=dataList , # inits=initsList , 
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

# Extract the posterior sample from JAGS:
muSample = mcmcChain[,"normMu"]
sigmaSample = mcmcChain[,"normSigma"] # BRugs gets sample from JAGS


# Make a graph using R commands:
openGraph(width=10,height=6)
layout( matrix( c(1,2) , nrow=1 ) )
source("plotPost.R")
histInfo = plotPost( muSample , xlim=c(0,0.5) , xlab=bquote(mu) )
histInfo = plotPost( sigmaSample , xlim=c(0,0.1) , xlab=bquote(sigma) )
saveGraph(file = paste0(savePath,"BinTruncNormBetaGamma"), type="eps")
