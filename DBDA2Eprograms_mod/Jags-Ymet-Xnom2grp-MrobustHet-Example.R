# Example for Jags-Ymet-Xnom2grp-MrobustHet.R 
#------------------------------------------------------------------------------- 
# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!
#------------------------------------------------------------------------------- 
# Load The data file 

# myDataFrame = read.csv( file="TwoGroupIQ.csv" )
# yName="Score"
# fileNameRoot = "TwoGroupIQrobustHet-" 
# RopeMuDiff=c(-0.5,0.5) ; RopeSdDiff=c(-0.5,0.5) ; RopeEff=c(-0.1,0.1)

#myDataFrame = read.csv( file="ShohatOphirKAMH2012dataReduced.csv" )
#xName="Group"
#yName="PreferenceIndex"
#fileNameRoot="Figures/FliesAndRats/ShohatOphirKAMH2012data-PI-"
#RopeMuDiff=c(-0.05,0.05) ; RopeSdDiff=c(-0.05,0.05) ; RopeEff=c(-0.2,0.2)

#myDataFrame = read.csv( file="ShohatOphirKAMH2012dataReduced.csv" )
#xName="Group"
#yName="GrandTotal"
#fileNameRoot="Figures/FliesAndRats/ShohatOphirKAMH2012data-GT-"
#RopeMuDiff=c(-1,1) ; RopeSdDiff=c(-1,1) ; RopeEff=c(-0.1,0.1)

#myDataFrame = read.csv( file="RatLives.csv" )
#xName="Group"
#yName="DaysLive"
#fileNameRoot = "Figures/FliesAndRats/RatLives-" 
#RopeMuDiff=c(-100,100) ; RopeSdDiff=c(-10,10) ; RopeEff=c(-1,1)

myDataFrame = read.csv( file="RatLives.csv" )
xName="Group"
myDataFrame = cbind( myDataFrame , DaysLiveSq = myDataFrame$DaysLive^2 )
yName="DaysLiveSq"
fileNameRoot = "Figures/FliesAndRats/RatLives-DaySq-" 
RopeMuDiff=c(-100000,100000) ; RopeSdDiff=c(-100000,100000) ; RopeEff=c(-1,1)

graphFileType = "eps" 
#------------------------------------------------------------------------------- 
# Load the relevant model into R's working memory:
source("Jags-Ymet-Xnom2grp-MrobustHet.R")
#------------------------------------------------------------------------------- 
# Generate the MCMC chain:
mcmcCoda = genMCMC( datFrm=myDataFrame , yName=yName , xName=xName ,
                    numSavedSteps=50000 , saveName=fileNameRoot )
#------------------------------------------------------------------------------- 
# Display diagnostics of chain, for specified parameters:
parameterNames = varnames(mcmcCoda) # get all parameter names
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
                saveName=fileNameRoot , saveType=graphFileType )
}
#------------------------------------------------------------------------------- 
# Get summary statistics of chain:
summaryInfo = smryMCMC( mcmcCoda , RopeMuDiff=RopeMuDiff , 
                        RopeSdDiff=RopeSdDiff , RopeEff=RopeEff ,
                        saveName=fileNameRoot )
show(summaryInfo)
# Display posterior information:
plotMCMC( mcmcCoda , datFrm=myDataFrame , yName=yName , xName=xName , 
          RopeMuDiff=RopeMuDiff , RopeSdDiff=RopeSdDiff , RopeEff=RopeEff ,
          pairsPlot=TRUE , saveName=fileNameRoot , saveType=graphFileType )
#------------------------------------------------------------------------------- 
