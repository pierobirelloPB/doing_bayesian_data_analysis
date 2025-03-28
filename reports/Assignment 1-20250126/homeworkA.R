set.seed(1)

p=list()  # list for storing the result/results

chain_probability_estimator = function(k=0.5, chain_length=6, n_flips=50, n_iters=10000)
{
  hit_v = rep(0,n_iters)
  for (i in 1:n_iters){    # loop for generating 1000   50 long binomial sequences
    binom_v = rbinom(n_flips, 1, k)
    countH=0
    countT=0
    hit=0
    for (j in 1:length(binom_v)){   # loop for checking wether a sequence contains a 6 long run
      if (binom_v[j] == 1){
        countH = countH + 1   #if it is head increment the head counter
        countT = 0            #and reset the tail counter
      }else{
        countH = 0            #if it is tail reset the head counter 
        countT = countT + 1   #and increment the tail counter
      }
      if (countH == chain_length || countT == chain_length){  #if one of the counter reaches 6
        hit_v[i] = 1          #put 1 to the corresponding element of the hit vector
        break                 #escape from the inner for loop
      }
    }
  }
  estimated_p = sum(hit_v)/n_iters
  return(estimated_p)
}

names = c('ub','b')
ks = c(0.5,0.7)

for (i in 1:2){p[names[i]]=chain_probability_estimator(k=ks[i])}

for (name in names) {
  print(sprintf("p_%s: %.3f", name, p[name]))
}


# for the 5. part of the exercise A

q=list(ub=0.6, b=0.4)   # biased and unbiased coin proportions in the bag
  
prob_6seq = p[["ub"]]*q[["ub"]] + p[["b"]]*q[["b"]] # evidence
print( sprintf('prob_6seq: %.2f%%', prob_6seq*100))
 
prob_biasG6seq = p[["b"]]*q[["b"]]/prob_6seq  # probability of biased coin Given to the six long sequence
print( sprintf('prob_biasG6seq: %.2f%%', prob_biasG6seq*100))


