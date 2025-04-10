# Bayesian Data Analysis 2025, Assignment 3, Exercise 1
#-------------------------------------------------------------------------------
source("DBDA2E-utilities.R")
#===============================================================================

# Improved initialization function
genMCMC = function(data, numSavedSteps=20000, saveName=NULL) {
  require(rjags)
  savePath = "Figures/Exercise1/2-"
  
  # LOAD DATA
  if (class(data)=="data.frame") {
    y = myData$y
  } else {
    y = data
  }
  
  # Do some checking that data make sense:
  if (any(y != floor(y))) { stop("All y values must be integers.") }
  
  # Compute length
  nBags = length(y)
  
  # Specify the data in a list
  dataList = list(y = y, nBags = nBags)
  
  # JAGS MODEL
  modelString = "
  model {
    # Likelihood 
    for (i in 1:nBags) {
      y[i] ~ dbin(p[b[i]], n[i])
      n[i] <- round(nContinuous[i])
      nContinuous[i] ~ dnorm(normMu, normTau)
      b[i] <- bBernoulli[i] + 1
      bBernoulli[i] ~ dbern(1-catTheta)
    }
    # Priors
    p[1] ~ dbeta(betaA, betaB)  # when b=0
    p[2] ~ dbeta(1, 1)          # when b=1
    catTheta ~ dbeta(1, 1)
    # Constants
    normMu <- 30
    normTau <- 1/(normSigma*normSigma)
    normSigma <- 1.5
    betaA <- 40
    betaB <- 10
  }
  "
  
  writeLines(modelString, con="modelExercise1.txt")
  
  # IMPROVED INITIALIZATION
  # Create multiple initialization lists to increase chances of convergence
  initsList = list()
  for (i in 1:3) { # for each chain
    initsList[[i]] = list(
      p = c(runif(1, 0.7, 0.9), runif(1, 0.6, 0.8)),  # randomize within reasonable range
      catTheta = runif(1, 0.2, 0.4),                  # randomize within reasonable range
      bBernoulli = rbinom(prob=0.7, size=1, n=nBags),
      nContinuous = pmax(10, rnorm(nBags, mean=30, sd=1.5)) # ensure positive values
    )
  }
  
  # RUN THE CHAINS
  parameters = c("catTheta", "p[1]", "p[2]")
  adaptSteps = 2000              # Increased for better adaptation
  burnInSteps = 10000            # Increased for better convergence
  nChains = 3
  thinSteps = 1
  nIter = ceiling((numSavedSteps * thinSteps) / nChains)
  
  # Create, initialize, and adapt the model with robust error handling
  tryCatch({
    jagsModel = jags.model("modelExercise1.txt", data=dataList, inits=initsList, 
                           n.chains=nChains, n.adapt=adaptSteps)
    
    cat("Burning in the MCMC chain...\n")
    update(jagsModel, n.iter=burnInSteps)
    
    cat("Sampling final MCMC chain...\n")
    codaSamples = coda.samples(jagsModel, variable.names=parameters, 
                               n.iter=nIter, thin=thinSteps)
    
    if (!is.null(saveName)) {
      save(codaSamples, file=paste(saveName, "Mcmc.Rdata", sep=""))
    }
    return(codaSamples)
  }, error = function(e) {
    cat("JAGS Error:", e$message, "\n")
    cat("Attempting to recover with alternative initialization...\n")
    
    # Alternative initialization as fallback
    altInitsList = list()
    for (i in 1:3) {
      altInitsList[[i]] = list(
        p = c(0.75, 0.65),
        catTheta = 0.3,
        bBernoulli = rep(0, nBags), # simplified initialization
        nContinuous = rep(30, nBags) # simplified initialization
      )
    }
    
    # Try again with simpler initialization
    jagsModel = jags.model("modelExercise1.txt", data=dataList, inits=altInitsList, 
                           n.chains=nChains, n.adapt=adaptSteps*2)
    update(jagsModel, n.iter=burnInSteps*2)
    codaSamples = coda.samples(jagsModel, variable.names=parameters, 
                               n.iter=nIter, thin=thinSteps)
    
    if (!is.null(saveName)) {
      save(codaSamples, file=paste(saveName, "Mcmc.Rdata", sep=""))
    }
    return(codaSamples)
  })
}

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
