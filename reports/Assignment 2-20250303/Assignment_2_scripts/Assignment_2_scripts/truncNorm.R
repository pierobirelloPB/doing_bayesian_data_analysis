
# Plot a truncated normal distribution

# Parameters
mean = 0.05
sd = 0.01
a = 0.0
b = 0.1

# Support
x = seq(a,b,length=1000)

# Compute the truncated normal PDF
pdfValues <- dnorm(x, mean = mean, sd = sd) / 
  (pnorm(b, mean = mean, sd = sd) - pnorm(a, mean = mean, sd = sd))

# Plot the exact density function
plot(x, pdfValues, type = "l", col = "blue", lwd = 2, 
     main = "Truncated Normal Distribution",
     xlab = "Value", ylab = "Density")