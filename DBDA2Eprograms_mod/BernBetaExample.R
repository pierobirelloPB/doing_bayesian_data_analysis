source("DBDA2E-utilities.R")  # Load definitions of graphics functions etc.
source("BernBeta.R")          # Load the definition of the BernBeta function

# Specify the prior:
t = 0.75             # Specify the prior MODE.
n = 25               # Specify the effective prior sample size.
a = t*(n-2) + 1      # Convert to beta shape parameter a.
b = (1-t)*(n-2) + 1  # Convert to beta shape parameter b.

Prior = c(a,b)       # Specify Prior as vector with the two shape parameters.

# Specify the data:
N = 20                         # The total number of flips.
z = 17                         # The number of heads.
Data = c(rep(0,N-z),rep(1,z))  # Convert N and z into vector of 0's and 1's.

openGraph(width=5,height=7)
posterior = BernBeta( priorBetaAB=Prior, Data=Data , plotType="Bars" , 
                      showCentTend="Mode" , showHDI=TRUE , showpD=FALSE )
saveGraph(file="Figures/BernBetaExample",type="png")

#---------------------------------------------------------------------------

source("DBDA2E-utilities.R")  # Load definitions of graphics functions etc.
source("BernBeta.R")          # Load the definition of the BernBeta function

# Specify the prior
a = 4
b = 4
Prior = c(a,b)

# Specify the data
N = 1
z = 1
Data = c(rep(1,z))

openGraph(width=5,height=7)
posterior = BernBeta( priorBetaAB=Prior, Data=Data , plotType="Bars" , 
                      showCentTend="Mode" , showHDI=TRUE , showpD=FALSE )
saveGraph(file="Figures/BernBetaExample6.1",type="png")
print(posterior)

#---------------------------------------------------------------------------

Data_2 = c(1)
posterior_2 = BernBeta( priorBetaAB = posterior, Data = Data_2, plotType="Bars" , 
                        showCentTend="Mode" , showHDI=TRUE , showpD=FALSE ) 
saveGraph(file="Figures/BernBetaExample6.1B",type="png")
print(posterior_2)

#---------------------------------------------------------------------------

Data_3 = c(0)
posterior_3 = BernBeta( priorBetaAB = posterior_2, Data = Data_3, plotType="Bars" , 
                                      showCentTend="Mode" , showHDI=TRUE , showpD=FALSE ) 
saveGraph(file="Figures/BernBetaExample6.1C",type="png")
print(posterior_3)

#---------------------------------------------------------------------------

# Specify the prior
a = 4
b = 4
Prior = c(a,b)

# Specify data
Data_1 = c(0)
Data_2 = c(1)
Data_3 = c(1)
posterior = BernBeta( priorBetaAB = BernBeta(
                                      priorBetaAB = BernBeta(
                                              priorBetaAB = Prior, Data = Data_1),
                                      Data = Data_2), Data = Data_3, 
                        plotType="Bars" , 
                        showCentTend="Mode" , showHDI=TRUE , showpD=FALSE ) 
saveGraph(file="Figures/BernBetaExample6.1D",type="png")
print(posterior)

#--------------------------------------------------------------------------

source("DBDA2E-utilities.R")  # Load definitions of graphics functions etc.
source("BernBeta.R")          # Load the definition of the BernBeta function

Prior = c(1,1)
N = 100+100
z = 58+57
Data = c(rep(0,N-z),rep(1,z)) 

posterior = BernBeta( priorBetaAB = Prior, Data = Data, plotType="Bars" , 
                      showCentTend="Mode" , showHDI=TRUE , showpD=FALSE )
saveGraph(file="Figures/Ex6.2B",type="pdf")
print

#----------------------------------------------------------------------------

source("DBDA2E-utilities.R")  # Load definitions of graphics functions etc.
source("BernBeta.R")          # Load the definition of the BernBeta function

Prior = c(0.1,0.1)
N = 5
z = 4
Data = c(rep(0,N-z),rep(1,z)) 

posterior = BernBeta( priorBetaAB = Prior, Data = Data, plotType="Bars" , 
                      showCentTend="Mode" , showHDI=TRUE , showpD=FALSE )
saveGraph(file="Figures/Ex6.4",type="pdf")
print(posterior)

#----------------------------------------------------------------------------

source("DBDA2E-utilities.R")  # Load definitions of graphics functions etc.
source("BernBeta.R")          # Load the definition of the BernBeta function

# Specify the prior:
t = 0.5            # Specify the prior MODE.
n = 1000               # Specify the effective prior sample size.
a = t*(n-2) + 1      # Convert to beta shape parameter a.
b = (1-t)*(n-2) + 1  # Convert to beta shape parameter b.
print(a)
print(b)

Prior = c(a,b)       # Specify Prior as vector with the two shape parameters.

# Specify the data:
N = 10                         # The total number of flips.
z = 9                         # The number of heads.
Data = c(rep(0,N-z),rep(1,z))  # Convert N and z into vector of 0's and 1's.

openGraph(width=5,height=7)
posterior = BernBeta( priorBetaAB=Prior, Data=Data , plotType="Bars" , 
                      showCentTend="Mode" , showHDI=TRUE , showpD=FALSE )
saveGraph(file="Figures/6.5",type="png")

#------------------------------------------------------------------------------

source("DBDA2E-utilities.R")  # Load definitions of graphics functions etc.
source("BernBeta.R")          # Load the definition of the BernBeta function

Prior = c(0.1,0.1)
N = 10
z = 9
Data = c(rep(0,N-z),rep(1,z)) 

posterior = BernBeta( priorBetaAB = Prior, Data = Data, plotType="Bars" , 
                      showCentTend="Mode" , showHDI=TRUE , showpD=FALSE )
saveGraph(file="Figures/6.5B",type="pdf")
print(posterior)

