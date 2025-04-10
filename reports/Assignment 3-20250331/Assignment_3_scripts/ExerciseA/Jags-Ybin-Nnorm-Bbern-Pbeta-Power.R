# Jags-Ybin-Nnorm-Bbern-Pbeta-Power.R
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!
fileNameRoot = "Jags-Ydich-Xnom1subj-MbernBeta-Power-" # for future use

# Load the functions genMCMC, smryMCMC, and plotMCMC:
# (This also sources DBDA2E-utilities.R)
source("Jags-Ybin-Nnorm-Bbern-Pbeta.R")

#-------------------------------------------------------------------------------
# Define function that assesses goal achievement for a single set of data:
goalAchievedForSample = function( data ) {
  # Generate the MCMC chain:
  mcmcCoda = genMCMC( data=data , numSavedSteps=10000 , saveName=NULL )
  # Check goal achievement. First, compute the HDI:
  p1HDI = HDIofMCMC( as.matrix(mcmcCoda[,"p[1]"]) )
  p2HDI = HDIofMCMC( as.matrix(mcmcCoda[,"p[2]"]) )
  # Define list for recording results:
  goalAchieved = list()
  # Goal: No overlap between HDIs
  goalAchieved = c(goalAchieved,
                    "noOverlap"=(( p1HDI[2] < p2HDI[1]) || (p2HDI[2] < p1HDI[1])) )
  # More goals can be inserted here if wanted...
  # Return list of goal results:
  return(goalAchieved)
}

#-------------------------------------------------------------------------------
# Specify hypothetical parameters:
p = c(0.8,0.7)
catTheta=0.3
normMu = 30
normSigma = 1.5

# Define proportion success list
proportionSuccessList = c()

# Iterate over sample size (number of bags):
for (sampleN in seq(1500, 2000, by=100)) {
  # Run a bunch of simulated experiments:
  nSimulatedDataSets = 10
  # Track number of times goal is achieved
  goalCount=0
  
  for ( simIdx in 1:nSimulatedDataSets ) {
    # Use tryCatch to handle errors and continue the loop
    tryCatch({
    # Generate random value from hypothesized parameter distribution:
    nContinuous = rnorm(n=sampleN,mean=normMu,sd=normSigma)
    n = round(nContinuous)
    bBernoulli = rbinom(n=sampleN,size=1,prob=1-catTheta)
    b = bBernoulli + 1
    # Generate random data based on parameter value:
    sampleY = sapply(1:length(n), function(i) {
      rbinom(n = 1, size = n[i], prob = p[b[i]])
    })
    # Do Bayesian analysis on simulated data:
    goalAchieved = goalAchievedForSample( sampleY )
    # Tally the results:
    if (!exists("goalTally")) { # if goalTally does not exist, create it
      goalTally=matrix( nrow=0 , ncol=length(goalAchieved) ) 
    }
    goalTally = rbind( goalTally , goalAchieved )
    # save( goalTally ,
    #       file="Jags-Ydich-Xnom1subj-MbernBeta-Power-goalTally.Rdata" )
    if (goalAchieved[["noOverlap"]] == TRUE) {
      goalCount = goalCount + 1
    }
    }, error = function(e) {
      # This block will execute if an error occurs
      cat("Error in simulation", simIdx, "for sample size", sampleN, ": ", e$message, "\n")
      # Continue with the next iteration of the loop
    })
  }
  
  # Calculate proportion of successes:
  proportionSuccess = goalCount / nSimulatedDataSets
  # Print
  cat("\n","\n")
  cat(sampleN, " COMPLETED ", "\n")
  cat("SampleN =", sampleN, " | Success rate:", round(proportionSuccess * 100, 2), "%\n")
  cat("\n","\n")
  # Store
  proportionSuccessList[as.character(sampleN)] = proportionSuccess
  # Check stopping condition:
  if (proportionSuccess > 0.8) {
    print(proportionSuccessList)
    cat("Stopping early: goal achieved in over 80% of simulations for sample size", sampleN, "\n")
    break
  }
  
}