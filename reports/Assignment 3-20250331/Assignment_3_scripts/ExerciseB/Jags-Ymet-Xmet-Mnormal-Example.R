# Bayesian Data Analysis 2025, Assignment 3, Exercise B
#-------------------------------------------------------------------------------
# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!
#-------------------------------------------------------------------------------
# Preliminaries
source("openGraphSaveGraph.R")
source("plotPost.R")
require(rjags)
fileNameRoot = "Figures/Mnormal-"
graphFileType = "eps"
# Load the relevant model into R's working memory:
source("Jags-Ymet-Xmet-Mnormal.R")

#-------------------------------------------------------------------------------
# LOAD DATA
myData = read.table('../../caschools.csv', header = TRUE, sep=',')
xName = "stratio" ; yName = "testscr"

# Summary
str(myData)
summary(myData)
# Hist and scatterplot
openGraph(width=12,height=6)
layout( matrix( c(1,2,3) , nrow=1 ) )
hist(myData[,xName])
hist(myData[,yName])
plot(myData[,xName], myData[,yName],
     xlab = "Student-Teacher Ratio",
     ylab = "Test Score"
)
saveGraph(file=paste0(fileNameRoot,"preliminary"),type="eps")

#------------------------------------------------------------------------------- 
# Generate the MCMC chain:
#startTime = proc.time()
mcmcCoda = genMCMC( data=myData , xName=xName , yName=yName , 
                    numSavedSteps=20000 , saveName=fileNameRoot )
#stopTime = proc.time()
#duration = stopTime - startTime
#show(duration)

#------------------------------------------------------------------------------- 
# Display diagnostics of chain, for specified parameters:
parameterNames = varnames(mcmcCoda) # get all parameter names
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}

#-------------------------------------------------------------------------------
# Get summary statistics of chain:
summaryInfo = smryMCMC( mcmcCoda , 
                        compValBeta1=0.0 , ropeBeta1=c(-0.5,0.5) ,
                        saveName=fileNameRoot )
show(summaryInfo)
# Display posterior information:
plotMCMC( mcmcCoda , data=myData , xName=xName , yName=yName , 
          compValBeta1=0.0 , ropeBeta1=c(-0.5,0.5) ,
          pairsPlot=TRUE , showCurve=FALSE ,
          saveName=fileNameRoot , saveType=graphFileType )
#------------------------------------------------------------------------------- 



