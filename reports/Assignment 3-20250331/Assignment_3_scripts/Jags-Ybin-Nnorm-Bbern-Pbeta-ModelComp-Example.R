# Bayesian Data Analysis 2025, Assignment 3, Exercise 1
#-------------------------------------------------------------------------------
# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!
#-------------------------------------------------------------------------------
# Preliminaries
source("openGraphSaveGraph.R")
source("plotPost.R")
require(rjags)
savePath = "Figures/Exercise1/c-"
graphFileType = "eps"
#-------------------------------------------------------------------------------
# LOAD DATA
myData = read.table(file='../tulips.txt',sep="\t")
yName = "no_tulips"
y=myData[,yName]
nBags = length(y)
dataList = list(y=y,nBags=nBags)
print(nBags)
# Plot histogram
openGraph(width=10,height=6)
hist(y)
saveGraph(file = paste0(savePath,"histY"), type="eps")
#-------------------------------------------------------------------------------
# JAGS MODEL

# Specify the model in JAGS language, save it as a string in R:
modelString = "
# JAGS model specification
model {
# Likelyhood 
for (i in 1:nBags) {
y[i] ~ dbin( p[b[i]], n[i] )
n[i] <- round(nContinuous[i])
nContinuous[i] ~ dnorm(normMu,normTau)
b[i] <- bBernoulli[i] + 1
bBernoulli[i] ~ dbern(1-catTheta) # equivalent to dcat(pCat[]) // pCat[1] = catTheta
}
# Priors
p[1] ~ dbeta(p1BetaA,p1BetaB)  # when b=0
p[2] ~ dbeta(1,1)          # when b=1
catTheta ~ dbeta(thetaBetaA[m],thetaBetaB[m])
# Constants
normMu <- 30
normTau <- 1/(normSigma*normSigma) #Convert to precision
normSigma <- 1.5
p1BetaA <- 40
p1BetaB <- 10
thetaBetaA[1] <- 1
thetaBetaB[1] <- 1
thetaBetaA[2] <- 15
thetaBetaB[2] <- 15
# Model Dummy Variable
m ~ dcat(mPriorProb)
mPriorProb[1] <- 0.5
mPriorProb[2] <- 0.5
}
# ... JAGS model specification ends.
" # close quote to end modelString

# Write the modelString to a file, using R commands:
writeLines(modelString,con="modelExercise1c.txt")

#-------------------------------------------------------------------------------
# INTIALIZE THE CHAIN
initsList = list( p=c(0.8,0.8), catTheta=0.5 , m=1 )

#-------------------------------------------------------------------------------
# RUN THE CHAINS

# Assign parameters
parameters = c( "catTheta","p[1]","p[2]","m" )   # The parameter(s) to be monitored.
adaptSteps = 1000              # Number of steps to "tune" the samplers.
burnInSteps = 5000            # Number of steps to "burn-in" the samplers.
nChains = 3                   # Number of chains to run.
numSavedSteps=50000           # Total number of steps in chains to save.
thinSteps=1                   # Number of steps to "thin" (1=keep every step).
nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains ) # Steps per chain.

# Create, initialize, and adapt the model:
jagsModel = jags.model( "modelExercise1c.txt" , data=dataList , inits=initsList , 
                        n.chains=nChains , n.adapt=adaptSteps )
# Burn-in:
cat( "Burning in the MCMC chain...\n" )
update( jagsModel , n.iter=burnInSteps )
# The saved MCMC chain:
cat( "Sampling final MCMC chain...\n" )
codaSamples = coda.samples( jagsModel , variable.names=parameters , 
                            n.iter=nIter , thin=thinSteps )

#-------------------------------------------------------------------------------
# EXAMINE THE RESULTS
mcmcChain = as.matrix( codaSamples )

# Extract the posterior sample from JAGS:
catThetaSample = mcmcChain[,"catTheta"]
p1Sample = mcmcChain[,"p[1]"]
p2Sample = mcmcChain[,"p[2]"]
pDiffSample = p1Sample-p2Sample
mSample = mcmcChain[,"m"]

# Compute the proportion of m at each index value:
pM1 = sum( mSample == 1 ) / length( mSample )
pM2 = 1 - pM1
print(paste0('model 1 prob:', pM1))

# Extract param values for each model index:
catThetaSampleM1 = catThetaSample[ mSample == 1 ]
catThetaSampleM2 = catThetaSample[ mSample == 2 ]
p1SampleM1 = p1Sample[ mSample == 1 ]
p1SampleM2 = p1Sample[ mSample == 2 ]
p2SampleM1 = p2Sample[ mSample == 1 ]
p2SampleM2 = p2Sample[ mSample == 2 ]
pDiffSampleM1 = p1SampleM1-p2SampleM1
pDiffSampleM2 = p1SampleM2-p2SampleM2

# Make a graph using R commands:
openGraph(width=10,height=6)
layout( matrix( c(1,2) , nrow=1 ) )
histInfo = plotPost( catThetaSampleM1 , xlim=c(0,1) , xlab=bquote("theta (m=1)") )
histInfo = plotPost( catThetaSampleM2 , xlim=c(0,1) , xlab=bquote("theta (m=2)") )
saveGraph(file = paste0(savePath,"catTheta"), type="eps")

# Make a graph using R commands:
openGraph(width=10,height=6)
layout( matrix( c(1,2,3) , nrow=1 ) )
histInfo = plotPost( p1SampleM1 , xlim=c(0,1) , xlab=bquote("p[1] (m=1)") )
histInfo = plotPost( p2SampleM1 , xlim=c(0,1) , xlab=bquote("p[2] (m=1)") )
histInfo = plotPost( pDiffSampleM1 , xlab=bquote("p[1]-p[2] (m=1)") , compVal=0. , ROPE=c(-0.1,0.1) )
saveGraph(file = paste0(savePath,"p[1]_p[2]_m1"), type="eps")

# Make a graph using R commands:
openGraph(width=10,height=6)
layout( matrix( c(1,2,3) , nrow=1 ) )
histInfo = plotPost( p1SampleM2 , xlim=c(0,1) , xlab=bquote("p[1] (m=2)") )
histInfo = plotPost( p2SampleM2 , xlim=c(0,1) , xlab=bquote("p[2] (m=2)") )
histInfo = plotPost( pDiffSampleM2 , xlab=bquote("p[1]-p[2] (m=2)") , compVal=0. , ROPE=c(-0.1,0.1) )
saveGraph(file = paste0(savePath,"p[1]_p[2]_m2"), type="eps")

# Make a graph using R commands:
openGraph(width=10,height=6)
#layout( matrix( c(1,3) , nrow=1 ) )
histInfo = plotPost( mSample , xlim=c(0.7,2.3), breaks=seq(0.9,2.1,0.2) , xlab="m" , main="Model Index" )
saveGraph(file = paste0(savePath,"mPost"), type="eps")
