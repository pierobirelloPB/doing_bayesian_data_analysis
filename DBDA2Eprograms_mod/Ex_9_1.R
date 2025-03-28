
source("DBDA2E-utilities.R")
#-------------------------------------------------------------------------------

pars1 = gammaShRaFromMeanSD(1,10)
s1=pars1$shape
r1=pars1$rate
pars2 = gammaShRaFromModeSD(1,10)
s2=pars2$shape
r2=pars2$rate
print(paste(s1,r1))
print(paste(s2,r2))

openGraph(height=7,width=7)
layout(matrix(1:3,ncol=1))
k=seq(0,200,length=10001)
plot( k , dgamma(k,s2,r2) , ylab="dgamma(k)" ,
      type="l" , main="Gamma Distrib’s (SD=10)" )
lines( k , dgamma(k,s1,r1) , col="skyblue" )
legend( "topright" , c("Mode 1","Mean 1") ,
        lty=c(1,1) , col=c("black","skyblue") , text.col=c("black", "skyblue") )
plot( k , dgamma(k,1.105125,0.105125) , ylab="dgamma(k)" ,
      type="l" , ylim=c(.07,.08) , main="Gamma Distrib’s (SD=10), zoomed in" )
lines( k , dgamma(k,0.01,0.01) , col="skyblue" )
legend( "topright" , c("Mode 1","Mean 1") ,
        lty=c(1,1) , col=c("black","skyblue") , text.col=c("black", "skyblue") )
plot( k , dgamma(k,1.105125,0.105125) , ylab="dgamma(k)" ,
      type="l" , ylim=c(0,8.0e-5) , main="Gamma Distrib’s (SD=10), zoomed in" )
lines( k , dgamma(k,0.01,0.01) , col="skyblue" )
legend( "topright" , c("Mode 1","Mean 1") ,
        lty=c(1,1) , col=c("black","skyblue") , text.col=c("black", "skyblue") )
saveGraph(file="Figures/Ex9.1")
