#### SIR Model ####

# How long does it take for X % of the population to get to the R group a function of alpha and beta?
# What happens if increase alpha only? What happens if increase beta?
# How does population size affect the length of the epidemic?

# Population Size
N <- 10000

# Probability of moving from S to I
alpha <- 0.005

# Probability of moving from I to R
beta <- 0.4

d <- 0.05

SuInReDe <- function(alpha,beta,N=1000) {
  suscept <- list(S = c(),I = c(),R = c(),D = c())
  
  suscept$S[1] <- N
  suscept$I[1] <- 1 # making assumption that starting with an infected
  suscept$R[1] <- 0
  suscept$D[1] <- 0
  
  t <- 1
  while (suscept$R[t] < (N+1)) {
    suscept$S[t+1] <- rbinom(1,suscept$S[t], (1-alpha)^(suscept$I[t]))
    
    suscept$R[t+1] <- suscept$R[t] + rbinom(1,suscept$I[t],beta)
    
    suscept$I[t+1] <- N + 1 - suscept$S[t+1] - suscept$R[t+1]
    suscept$D[t+1] <- suscept$D[t] + rbinom(1,suscept$R[t],d)
    t <- t + 1
  }
  for (i in suscept$R) {
    print(i)
    suscept$D[i] <- rbinom(1,suscept$R[i+1],d)
  }
  
  if (suscept$R[t] == (N+1)) {
    suscept$S[(length(suscept$S))] <- suscept$R[(length(suscept$R))]
    suscept$R[(length(suscept$R))] <- 0
  }
  # t <- t+1
  suscept$I[(length(suscept$I))] <- 1
  
  while (suscept$R[(length(suscept$R))] < (N+1)) {
    suscept$S[t+1] <- rbinom(1,suscept$S[t], (1-alpha)^(suscept$I[t]))
    
    suscept$R[t+1] <- suscept$R[t] + rbinom(1,suscept$I[t],beta)
    
    suscept$I[t+1] <- N + 1 - suscept$S[t+1] - suscept$R[t+1]
    
    t <- t + 1
  }
  
  # suscept$S[(length(suscept$S))] <- suscept$R[(length(suscept$R))]
  return(list(SIR = suscept, t = length(suscept$S)))
}
SIR(alpha,beta,N)
