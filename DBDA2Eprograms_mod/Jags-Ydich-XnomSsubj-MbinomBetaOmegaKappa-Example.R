# Example for Jags-Ydich-XnomSsubj-MbinomBetaOmegaKappa.R 
#------------------------------------------------------------------------------- 
# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!
#------------------------------------------------------------------------------- 
# Load The data 
#myData = read.csv("TherapeuticTouchData.csv")
# N.B.: The functions below expect the data to be a data frame, 
# with one component named y being a vector of integer 0,1 values,
# and one component named s being a factor of subject identifiers.

# Create vectors for each group
S01 = c(rep(1, 30), rep(0, 70))
S02 = c(rep(1, 40), rep(0, 60))
S03 = c(rep(1, 50), rep(0, 50))
S04 = c(rep(1, 60), rep(0, 40))
S05 = c(rep(1, 70), rep(0, 30))

# Create the data frame
myData = data.frame(
  y = c(S01, S02, S03, S04, S05),
  s = rep(c("S01", "S02", "S03", "S04", "S05"), each = 100)
)
print(myData)
#------------------------------------------------------------------------------- 
# Load the relevant model into R's working memory:
source("Jags-Ydich-XnomSsubj-MbinomBetaOmegaKappa.R")
#------------------------------------------------------------------------------- 
# Optional: Specify filename root and graphical format for saving output.
# Otherwise specify as NULL or leave saveName and saveType arguments 
# out of function calls.
fileNameRoot = "Figures/Hierarchical/MLEcomp-Jags-Ydich-XnomSsubj-MbinomBetaOmegaKappa-" 
graphFileType = "pdf" 
#------------------------------------------------------------------------------- 
# Generate the MCMC chain:
startTime = proc.time()
mcmcCoda = genMCMC( data=myData , sName="s" , yName="y" , 
                    numSavedSteps=20000 , saveName=fileNameRoot ,
                    thinSteps=10 )
stopTime = proc.time()
show( stopTime-startTime )
#------------------------------------------------------------------------------- 
# Display diagnostics of chain, for specified parameters:
parameterNames = varnames(mcmcCoda) # get all parameter names for reference
for ( parName in parameterNames[c(1:3,length(parameterNames))] ) { 
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
                saveName=fileNameRoot , saveType=graphFileType )
}
#------------------------------------------------------------------------------- 
# Get summary statistics of chain:
summaryInfo = smryMCMC( mcmcCoda , compVal=0.5 , 
                        diffIdVec=c(1,3,5), compValDiff=0.0, 
                        saveName=fileNameRoot )
# Display posterior information:
plotMCMC( mcmcCoda , data=myData , sName="s" , yName="y" , 
          compVal=0.5 , #rope=c(0.45,0.55) ,
          diffIdVec=c(1,3,5), compValDiff=0.0, #ropeDiff = c(-0.05,0.05) ,
          saveName=fileNameRoot , saveType=graphFileType )
#------------------------------------------------------------------------------- 
