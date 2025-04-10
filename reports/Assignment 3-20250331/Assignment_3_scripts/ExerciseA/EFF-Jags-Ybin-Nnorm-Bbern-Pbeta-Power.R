# Jags-Ybin-Nnorm-Bbern-Pbeta-Power.R
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!
fileNameRoot = "Jags-Ydich-Xnom1subj-MbernBeta-Power-" # for future use

# Load the functions genMCMC, smryMCMC, and plotMCMC:
# (This also sources DBDA2E-utilities.R)
source("Jags-Ybin-Nnorm-Bbern-Pbeta.R")

#-------------------------------------------------------------------------------
# Improved power analysis
goalAchievedForSample = function(data) {
  # Try to generate the MCMC chain with error handling
  tryCatch({
    mcmcCoda = genMCMC(data=data, numSavedSteps=10000, saveName=NULL)
    
    # Check for convergence issues
    gd = gelman.diag(mcmcCoda)
    if (any(gd$psrf > 1.1)) {
      warning("Potential convergence issues detected")
    }
    
    # Compute the HDI
    p1HDI = HDIofMCMC(as.matrix(mcmcCoda[,"p[1]"]))
    p2HDI = HDIofMCMC(as.matrix(mcmcCoda[,"p[2]"]))
    
    # Define list for recording results
    goalAchieved = list()
    
    # Goal: No overlap between HDIs
    goalAchieved = c(goalAchieved,
                     "noOverlap"=((p1HDI[2] < p2HDI[1]) || (p2HDI[2] < p1HDI[1])))
    
    return(goalAchieved)
  }, error = function(e) {
    cat("Error in MCMC analysis:", e$message, "\n")
    # Return NA for this simulation
    return(list("noOverlap"=NA))
  })
}

# Main power analysis loop with better data generation
powerAnalysis = function() {
  # Specify hypothetical parameters
  p = c(0.8, 0.7)
  catTheta = 0.3
  normMu = 30
  normSigma = 1.5
  
  proportionSuccessList = c()
  
  # Iterate over sample size (number of bags)
  for (sampleN in seq(200, 500, by=100)) {
    nSimulatedDataSets = 10
    goalCount = 0
    validSimulations = 0
    
    for (simIdx in 1:nSimulatedDataSets) {
      cat("Running simulation", simIdx, "of", nSimulatedDataSets, "for sample size", sampleN, "\n")
      
      # Generate better data with appropriate checks
      tryCatch({
        # Generate random value from hypothesized parameter distribution
        nContinuous = rnorm(n=sampleN, mean=normMu, sd=normSigma)
        n = round(pmax(5, nContinuous))  # Ensure n is at least 5
        bBernoulli = rbinom(n=sampleN, size=1, prob=1-catTheta)
        b = bBernoulli + 1
        
        # Generate random data based on parameter value
        sampleY = sapply(1:length(n), function(i) {
          rbinom(n=1, size=n[i], prob=p[b[i]])
        })
        
        # Verify data validity
        if (all(is.finite(sampleY)) && all(sampleY >= 0) && all(sampleY <= n)) {
          # Do Bayesian analysis on simulated data
          goalAchieved = goalAchievedForSample(sampleY)
          
          # Only count valid results
          if (!is.na(goalAchieved[["noOverlap"]])) {
            validSimulations = validSimulations + 1
            if (goalAchieved[["noOverlap"]] == TRUE) {
              goalCount = goalCount + 1
            }
          }
        } else {
          cat("Generated invalid data in simulation", simIdx, "\n")
        }
      }, error = function(e) {
        cat("Error in simulation", simIdx, "for sample size", sampleN, ":", e$message, "\n")
      })
    }
    
    # Calculate proportion of successes
    if (validSimulations > 0) {
      proportionSuccess = goalCount / validSimulations
      cat("\nSampleN =", sampleN, "| Success rate:", round(proportionSuccess * 100, 2), 
          "% (", goalCount, "out of", validSimulations, "valid simulations)\n\n")
      
      proportionSuccessList[as.character(sampleN)] = proportionSuccess
      
      # Check stopping condition
      if (proportionSuccess > 0.8 && validSimulations >= nSimulatedDataSets*0.8) {
        print(proportionSuccessList)
        cat("Stopping early: goal achieved in over 80% of valid simulations for sample size", sampleN, "\n")
        break
      }
    } else {
      cat("No valid simulations completed for sample size", sampleN, "\n")
    }
  }
  
  return(proportionSuccessList)
}

# Run the power analysis
results = powerAnalysis()
print(results)