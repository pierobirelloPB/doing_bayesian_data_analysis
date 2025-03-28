
# left tail p-value for z, given a 6-sided die (stopping criterion: fixed N)
N = 45 ; z = 3 ; theta = 1/6
lowTailZ = 0:z
lowTailP = sum( choose(N,lowTailZ) * theta^lowTailZ * (1-theta)^(N-lowTailZ) )
print(lowTailP)
print(lowTailP*2)

# Right tail p-value for N (stopping criterion: threshold z)
N = 45 ; z = 3 ; theta = 1/6
highTailN = z:(N-1)
highTailP = 1 - sum( z/highTailN * choose(highTailN,z) * theta^z * (1-theta)^(highTailN-z) )
print(highTailP)
print(highTailP*2)

# Confidence intervals (stopping criterion: fixed N)
for ( theta in seq( 0.170 , 0.190 , 0.001) ) {
  show( c(
    theta ,
    2*sum( choose(N,lowTailZ) * theta^lowTailZ * (1-theta)^(N-lowTailZ) )
  ))
}
highTailZ = z:N
for ( theta in seq( 0.005 , 0.020 , 0.001) ) {
  show( c(
    theta ,
    2*sum( choose(N,highTailZ) * theta^highTailZ * (1-theta)^(N-highTailZ) )
  ))
}

print('')

# Confidence intervals (stopping criterion: threshold z)
highTailN = z:(N-1)
for ( theta in seq( 0.140 , 0.160 , 0.001) ) {
  show( c(
    theta ,
    2* (1 - sum( z/highTailN * choose(highTailN,z) * theta^z * (1-theta)^(highTailN-z) ))
  ))
}
lowTailN = z:N
for ( theta in seq( 0.005 , 0.020 , 0.001) ) {
  show( c(
    theta ,
    2* sum( z/lowTailN * choose(lowTailN,z) * theta^z * (1-theta)^(lowTailN-z) )
  ))
}

print("")

# Stopping determined by time: multiple equiprobable N
N = 45 ; z = 3 ; theta = 1/6
# Specify possible N values:
Nposs = 40:50
# Specify probability of each N (here all equal):
Nprob = rep(1,length(Nposs)) ; Nprob = Nprob/sum(Nprob)
# For each possible N, compute p value, and compute the weighted total p:
totalP = 0
for ( i in 1:length(Nposs) ) {
  thisN = Nposs[i]
  # For this N, determine the max z that is in the low tail:
  thisZ = max( (0:thisN)[ (0:thisN)/thisN <= z/N ] )
  lowTailZ = 0:thisZ
  thisP = 2*sum( choose(thisN,lowTailZ) * theta^lowTailZ * (1-theta)^(thisN-lowTailZ) )
  totalP = totalP + Nprob[i] * thisP
  show( c( thisN , thisP ) )
}
show( totalP )