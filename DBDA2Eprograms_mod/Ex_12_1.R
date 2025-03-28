source("DBDA2E-utilities.R")
source("BernBeta.R")

z=7 ; N=24

BernoulliLikelyhood = function(z,N,theta){
  return(theta^z * (1-theta)^(N-z))
}

# Consider a spike prior
thetaNull = 0.5
print(BernoulliLikelyhood(z,N,theta=thetaNull))

# Consider a narrow prior centered in thetaNull
a=2000 ; b=2000
openGraph(width=5,height=7)
BernBeta( c(a,b) , c(rep(0,N-z),rep(1,z)) , ROPE=c(0.48,0.52) ,
          plotType="Bars" , showCentTend="Mode" , showHDI=TRUE , showpD=TRUE )
saveGraph(file="Figures/Ex_12_1", type="pdf")

# Consider a nearly Haldane Prior
a=0.01 ; b=0.01
openGraph(width=5,height=7)
BernBeta( c(a,b) , c(rep(0,N-z),rep(1,z)) , ROPE=c(0.48,0.52) ,
          plotType="Bars" , showCentTend="Mode" , showHDI=TRUE ,showpD=TRUE )
saveGraph(file="Figures/Ex_12_1_C", type="pdf")

# Bayes factor
BF = 2.87e-09/BernoulliLikelyhood(z,N,theta=thetaNull)
print(BF)

# Mild Beta prior
a=2 ; b=4
openGraph(width=5,height=7)
BernBeta( c(a,b) , c(rep(0,N-z),rep(1,z)) , ROPE=c(0.48,0.52) ,
          plotType="Bars" , showCentTend="Mode" , showHDI=TRUE , showpD=TRUE )
saveGraph(file="Figures/Ex_12_1_E",type="pdf")
