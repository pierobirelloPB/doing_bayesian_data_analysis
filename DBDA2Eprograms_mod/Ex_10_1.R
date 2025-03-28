source("DBDA2E-utilities.R")

# parameters and data
w1 = 0.25
w2 = 0.75
k = 202
z = 7
N = 10
pm1 = .5
pm2 = 1-pm1

# Beta a,b
ab1 = betaABfromModeKappa(w1,k)
ab2 = betaABfromModeKappa(w2,k)
print(ab1)

# Likelyhood
pD = function(z,N,a,b) { beta(z+a,N-z+b) / beta(a,b) }

# Posterior
pm1_given_D = pD(z,N,ab1$a,ab1$b) * pm1
pm2_given_D = pD(z,N,ab2$a,ab2$b) * pm2

# Evidence
evidence = pm1_given_D + pm2_given_D

# Posterior
pm1_given_D = pm1_given_D/evidence
pm2_given_D = pm2_given_D/evidence

print(pm1_given_D)
print(pm2_given_D)