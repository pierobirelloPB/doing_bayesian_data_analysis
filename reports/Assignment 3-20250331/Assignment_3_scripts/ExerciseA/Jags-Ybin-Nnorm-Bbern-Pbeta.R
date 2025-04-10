# Bayesian Data Analysis 2025, Assignment 3, Exercise 1
#-------------------------------------------------------------------------------
source("DBDA2E-utilities.R")
#===============================================================================

genMCMC = function( data , numSavedSteps=20000 , saveName=NULL ) {
  require(rjags)
  savePath = "Figures/Exercise1/2-"
  
  # LOAD DATA
  if ( class(data)=="data.frame" ) {  # If data is a data.frame
    y = myData$y                      # then pull out the column named y
  } else {                            # else
    y = data                          # rename the data as y.
  }
  # Do some checking that data make sense:
  if ( any( y!=floor(y) ) ) { stop("All y values must be integers.") }
  # Compute length
  nBags = length(y)
  # Specify the data in a list, for later shipment to JAGS:
  dataList = list(y = y , nBags = nBags)
  # Plot histogram
  #openGraph(width=10,height=6)
  #hist(y)
  #saveGraph(file = paste0(savePath,"histY"), type="eps")
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
  p[1] ~ dbeta(betaA,betaB)  # when b=0
  p[2] ~ dbeta(1,1)          # when b=1
  catTheta ~ dbeta(1,1)
  # Constants
  normMu <- 30
  normTau <- 1/(normSigma*normSigma) #Convert to precision
  normSigma <- 1.5
  betaA <- 40
  betaB <- 10
  }
  # ... JAGS model specification ends.
  " # close quote to end modelString
  
  # Write the modelString to a file, using R commands:
  writeLines(modelString,con="modelExercise1.txt")
  
  #-------------------------------------------------------------------------------
  # INTIALIZE THE CHAIN
  initsList = list( p=c(0.8,0.7), catTheta=0.3,
                    bBernoulli = rbinom(prob=0.7, size=1, n=nBags),  # Initialize bBernoulli with 0s or 1s
                    nContinuous = rnorm(nBags, mean=30, sd=1.5)  # Initialize nContinuous with reasonable values
                  )
  
  #-------------------------------------------------------------------------------
  # RUN THE CHAINS
  
  # Assign parameters
  parameters = c( "catTheta","p[1]","p[2]" )   # The parameter(s) to be monitored.
  adaptSteps = 1000              # Number of steps to "tune" the samplers.
  burnInSteps = 5000            # Number of steps to "burn-in" the samplers.
  nChains = 3                   # Number of chains to run.
  numSavedSteps=numSavedSteps   # Total number of steps in chains to save.
  thinSteps=1                   # Number of steps to "thin" (1=keep every step).
  nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains ) # Steps per chain.
  
  # Create, initialize, and adapt the model:
  jagsModel = jags.model( "modelExercise1.txt" , data=dataList , inits=initsList , 
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
  if ( !is.null(saveName) ) {
    save( codaSamples , file=paste(saveName,"Mcmc.Rdata",sep="") )
  }
  return( codaSamples )

} # Close genMCMC

#===============================================================================
smryMCMC = function(  codaSamples , compVal=NULL , rope=NULL , saveName=NULL ) {
  summaryInfo = NULL
  mcmcMat = as.matrix(codaSamples,chains=TRUE)
  summaryInfo = rbind( summaryInfo , 
                       "catTheta" = summarizePost( mcmcMat[,"catTheta"] , 
                                                compVal=compVal , ROPE=rope ) )
  if ( !is.null(saveName) ) {
    write.csv( summaryInfo , file=paste(saveName,"SummaryInfo.csv",sep="") )
  }
  show( summaryInfo )
  return( summaryInfo )
}

#===============================================================================
plotMCMC = function(codaSamples ,
                    savePath = "Figures/Exercise1/c-",
                    graphFileType = "eps" ) {
  
  source("openGraphSaveGraph.R")
  source("plotPost.R")

  # EXAMINE THE RESULTS.
  mcmcChain = as.matrix( codaSamples )
  
  # Extract the posterior sample from JAGS:
  catThetaSample = mcmcChain[,"catTheta"]
  p1Sample = mcmcChain[,"p[1]"]
  p2Sample = mcmcChain[,"p[2]"]
  pDiffSample = p1Sample - p2Sample
  
  # Make a graph using R commands:
  openGraph(width=10,height=6)
  #layout( matrix( c(1,3) , nrow=1 ) )
  histInfo = plotPost( catThetaSample , xlim=c(0,1) , xlab=bquote("theta") )
  saveGraph(file = paste0(savePath,"catTheta"), type="eps")
  
  # Make a graph using R commands:
  openGraph(width=10,height=6)
  layout( matrix( c(1,2,3) , nrow=1 ) )
  histInfo = plotPost( p1Sample , xlim=c(0,1) , xlab=bquote("p[1]") )
  histInfo = plotPost( p2Sample , xlim=c(0,1) , xlab=bquote("p[2]") )
  histInfo = plotPost( pDiffSample , xlab=bquote("p[1]-p[2]") , compVal=0. , ROPE=c(-0.1,0.1) )
  saveGraph(file = paste0(savePath,"p[1]_p[2]"), type="eps")

} # Close plotMCMC
