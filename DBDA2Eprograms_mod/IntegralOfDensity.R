source("DBDA2E-utilities.R")

# Graph of normal probability density function, with comb of intervals.
xlow  = 0 # Specify low end of x-axis.
xhigh = 1 # Specify high end of x-axis.
dx = (xhigh-xlow)/1000               # Specify interval width on x-axis
# Specify comb of points along the x axis:
x = seq( from = xlow , to = xhigh , by = dx )
# Compute y values, i.e., probability density at each value of x:
y = 6*x*(1-x)
# Plot the function. "plot" draws the intervals. "lines" draws the bell curve.
openGraph(width=7,height=5)
plot( x , y , type="h" , lwd=1 , cex.axis=1.5
	, xlab="x" , ylab="p(x)" , cex.lab=1.5 
	, main="Probability Density" , cex.main=1.5 )
lines( x , y , lwd=3 ,  col="skyblue" )
# Approximate the integral as the sum of width * height for each interval.
area = sum( dx * y )
print(area)
saveGraph( file = "Figures/IntegralOfDensity" , type="pdf" )


# # Display info in the graph.
# text( meanval-sdval , .9*max(y) , bquote( paste(mu ," = " ,.(meanval)) )
#       , adj=c(1,.5) , cex=1.5 )
# text( meanval-sdval , .75*max(y) , bquote( paste(sigma ," = " ,.(sdval)) )
#       , adj=c(1,.5) , cex=1.5 )
# text( meanval+sdval , .9*max(y) , bquote( paste(Delta , "x = " ,.(dx)) )
#       , adj=c(0,.5) , cex=1.5 )
# text( meanval+sdval , .75*max(y) ,
#       bquote( sum({},x,{}) *" "* Delta *"x p(x) = "* .(signif(area,3)) ) ,
#       adj=c(0,.5) , cex=1.5 )
# # Save the plot to an EPS file.
