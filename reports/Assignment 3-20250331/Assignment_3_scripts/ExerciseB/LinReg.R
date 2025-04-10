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
savePath = "Figures/"
graphFileType = "eps"
#-------------------------------------------------------------------------------
# LOAD DATA
data = read.table('../../caschools.csv', header = TRUE, sep=',')
str(data)
summary(data)
# Hist and scatterplot
openGraph(width=12,height=6)
layout( matrix( c(1,2,3) , nrow=1 ) )
hist(data$stratio)
hist(data$testscr)
plot(data$stratio, data$testscr,
     xlab = "Student-Teacher Ratio",
     ylab = "Test Score"
)
saveGraph(file=paste0(savePath,"preliminary"),type="eps")

