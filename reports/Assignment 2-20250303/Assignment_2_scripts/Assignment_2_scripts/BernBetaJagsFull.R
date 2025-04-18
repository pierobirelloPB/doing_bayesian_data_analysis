rm(list = ls())
graphics.off()
source("openGraphSaveGraph.R")
save_path = "Figures/Task3/A"

require(rjags)         # Kruschke, J. K. (2011). Doing Bayesian Data Analysis:
# A Tutorial with R and BUGS. Academic Press / Elsevier.
#------------------------------------------------------------------------------
# THE MODEL.

# Specify the model in JAGS language, but save it as a string in R:
modelString = "
# JAGS model specification begins ...
model {
# Likelihood:
for ( i in 1:nStores ) {
y[i] ~ dbin( theta,50*10 )
}
# Prior distribution:
theta ~ dnorm( 0.05,1/(0.01)^2 )T( 0.0,0.1 )
#theta ~ dnorm( 0.1,1/(0.02)^2 )T( 0.05,0.15 ) 
}
# ... JAGS model specification ends.
" # close quote to end modelString

# Write the modelString to a file, using R commands:
writeLines(modelString,con="model.txt")

trunc_low = 0.0
trunc_high = 0.1
dataN = 50*10
mu = 0.05
sigma = 0.01

#------------------------------------------------------------------------------
# THE DATA.

# Specify the data in R, using a list format compatible with JAGS:
dataList = list(
  nStores = 5 ,
  y = c( 7,5,4,4,2 )*10
)

#------------------------------------------------------------------------------
# INTIALIZE THE CHAIN.

# Can be done automatically in jags.model() by commenting out inits argument.
# Otherwise could be established as:
# initsList = list( theta = sum(dataList$y)/length(dataList$y) )

#------------------------------------------------------------------------------
# RUN THE CHAINS.

parameters = c( "theta" )     # The parameter(s) to be monitored.
adaptSteps = 500              # Number of steps to "tune" the samplers.
burnInSteps = 1000            # Number of steps to "burn-in" the samplers.
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
# resulting codaSamples object has these indices: 
#   codaSamples[[ chainIdx ]][ stepIdx , paramIdx ]


#------------------------------------------------------------------------------
# EXAMINE THE RESULTS.

# Convert coda-object codaSamples to matrix object for easier handling.
# But note that this concatenates the different chains into one long chain.
# Result is mcmcChain[ stepIdx , paramIdx ]
mcmcChain = as.matrix( codaSamples )

thetaSample = mcmcChain

# Make a graph using R commands:
openGraph(width=10,height=6)
layout( matrix( c(1,2) , nrow=1 ) )
plot( thetaSample[1:500] , 1:length(thetaSample[1:500]) , type="o" ,
      xlim=c(trunc_low,trunc_high) , xlab=bquote(theta) , ylab="Position in Chain" ,
      cex.lab=1.25 , main="JAGS Results" , col="skyblue" )
source("plotPost.R")
histInfo = plotPost( thetaSample , xlim=c(trunc_low,trunc_high) , xlab=bquote(theta) )
saveGraph( file=paste0(save_path,"BernBetaJagsFullPost") , type="eps" )

# Posterior prediction:
# For each step in the chain, use posterior theta to flip a coin:
chainLength = length( thetaSample )
yPred = rep( NULL , chainLength )  # define placeholder for flip results
for ( stepIdx in 1:chainLength ) {
  pHead = thetaSample[stepIdx]
  #yPred[stepIdx] = sample( x=c(0,1), prob=c(1-pHead,pHead), size=1 )
  yPred[stepIdx] = rbinom( n=1,size=50,prob=pHead )
}
# Jitter the y values for plotting purposes:
yPredJittered = yPred + runif( length(yPred) , -.1 , +.1 )
# Now plot the jittered values:
openGraph(width=5,height=5.5)
par( mar=c(3.5,3.5,2.5,1) , mgp=c(2,0.7,0) )
plot( thetaSample[1:500] , yPredJittered[1:500] , xlim=c(trunc_low,trunc_high) ,
      main="posterior predicted sample" ,
      xlab=expression(theta) , ylab=expression(hat(y)*" "*(jittered)) , 
      col="skyblue" )
points( mean(thetaSample) , mean(yPred) , pch="+" , cex=2 , col="skyblue" )
text( mean(thetaSample) , mean(yPred) ,
      bquote( mean(hat(y)) == .(signif(mean(yPred),2)) ) ,
      adj=c(1.2,.5) )
text( mean(thetaSample) , mean(yPred) , srt=90 ,
      bquote( mean(theta) == .(signif(mean(thetaSample),2)) ) ,
      adj=c(1.2,.5) )
#abline( trunc_low , trunc_high , lty="dashed" , lwd=2 )
saveGraph( file=paste0(save_path,"BernBetaJagsFullPred") , type="eps" )

#------------------------------------------------------------------------------
# COMPUTE PROBABILITY OF DATA

# Define truncated normal
truncNorm = function(x,mean,sd,a,b) {
  pdfValues = dnorm(x, mean = mean, sd = sd) / 
  (pnorm(b, mean = mean, sd = sd) - pnorm(a, mean = mean, sd = sd))
  return(pdfValues)
  }

# Compute mean and standard deviation of MCMC values:
meanTheta = mean(thetaSample)
sdTheta = sd(thetaSample)

# Compute 1/p(D):
logOneOverPD = mean( log( truncNorm( thetaSample,meanTheta,sdTheta,trunc_low,trunc_high ) ) - # h(theta_i)
                    ( rowSums( dbinom( dataList$y,dataN,thetaSample, log=TRUE ) ) +         # p(D|theta_i)
                  log( truncNorm( thetaSample,mu,sigma,trunc_low,trunc_high ) ) ) )      # p(theta_i)
logPD = -(logOneOverPD)
show(logPD)
