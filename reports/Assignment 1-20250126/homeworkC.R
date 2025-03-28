# Define variables for the two bags
p_k_given_M1 = c(0.5, 0.5)  # probability of k given model 1
k_M1 = c(0.7, 0.5)  # k values in model 1
p_k_given_M2 = rep(1/9, 9)  # probability of k given model 2
k_M2 = seq(0.1, 0.9, by=0.1)  # k values in model 2

# Single lists
p_k = list(p_k_given_M1, p_k_given_M2)
k = list(k_M1, k_M2)

# Tosses data
y = rbind(
  c(0, 1, 0, 0, 1, 0, 1, 0),
  c(0, 1, 1, 1, 0, 1, 1, 1)
)

# Define bernoulli distribution
bernoulli_seq = function(y, k) {
  prod(k^y * (1 - k)^(1 - y))
}

# Define likelihood of model alpha
likelihood = function(y, alpha) {
  sum(sapply(1:length(k[[alpha]]), function(i) {
    bernoulli_seq(y, k[[alpha]][i]) * p_k[[alpha]][i]
  }))
}

# Calculate likelihoods
likelihood_M1 = prod(apply(y, 1, function(y_) likelihood(y_, alpha=1)))
likelihood_M2 = prod(apply(y, 1, function(y_) likelihood(y_, alpha=2)))
K = likelihood_M1 / likelihood_M2

# Print results
print(paste("Likelihood M1:", likelihood_M1))
print(paste("Likelihood M2:", likelihood_M2))
print(paste("K:", K))