
# Plot prior
theta = seq(0,1,length=501);
plot(theta , (cos(4*pi*theta)+1) ** 2/1.5 )

# Save
fileNameRoot = "Figures/BernMetrop"
saveGraph(file = paste0(fileNameRoot, "CosinePrior"), type = "eps")