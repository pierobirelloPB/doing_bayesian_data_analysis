set.seed(1)
 
# uncomment for part 9: 
#layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))

#mean_beta=rep(NaN,3)
#sd_beta=rep(NaN,3)

library(Hmisc)

# function for extracting samples, plotting and getting summary statistics
# given parameters a and b for the Beta distribution
beta_hist = function(a = 10, b= 7)
{
beta_v = rbeta( 100000 , a , b ) # Beta distribution
hist(beta_v, # Histogram
     breaks = seq(0, 1, by = 0.05),
     freq = FALSE,                
     xlim = c(0, 1),                 
     ylim = c(0, 11),                
     col = "blue",                   
     xlab = "beta hist"
     )             
mean_beta = mean(beta_v)  # mean of beta_v
sd_beta = sd(beta_v) #sd of beta_v
abline(v=mean_beta, col="red", lwd=4)
text(x = mean_beta+0.15, y = 7.5, labels = sprintf("mean: %.3f", mean_beta))
return(list(mean=mean_beta,sd=sd_beta))
}

par(mfrow = c(2, 2)) # 2x2 plotting grid

# define iterables
a = c(10,50,100)
b = c(7,35,70)
mean_beta = c()
sd_beta = c()

# iterate
for (i in 1:3) {
  result = beta_hist(a[i],b[i])
  mean_beta[i] = result$mean
  sd_beta[i] = result$sd
}

print(mean_beta)

# Errorbar plot
x_vals = a + b
errbar(x_vals, mean_beta, 
       yplus = mean_beta + sd_beta, 
       yminus = mean_beta - sd_beta, 
       col = "red", 
       errbar.col = "blue",
       ylim = c(0.45,0.75),
       xlab = "a + b", 
       ylab = "mean_beta")
lines(x_vals, mean_beta, col = "red")
title("mean of the Beta distribution,\n as a function of (a+b)")

dev.print(file="ExB.pdf", pdf) #, height = 800, width=800)
dev.off()
