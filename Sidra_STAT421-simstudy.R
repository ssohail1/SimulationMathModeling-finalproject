# ************************************************************************
## SIR model with four different scenarios

# low infection rate and low recovery
# # Prob S to I
alpha <- 0.08
# # Prob I to R
beta <- 0.03

# high infection rate and high recovery
# # Prob S to I
alpha2 <- 0.2
# # Prob I to R
beta2 <- 0.5

# high infection rate and low recovery
# # Prob S to I
alpha3 <- 0.7
# # Prob I to R
beta3 <- 0.01

# low infection rate and high recovery
# # Prob S to I
alpha4 <- 0.02
# # Prob I to R
beta4 <- 0.8

SIR <- function(alpha,beta,N) {
  
  suscept <- list() # list with SIR
  suscept$S[1] <- N - 1
  suscept$I[1] <- 1
  suscept$R[1] <- 0
  t_1 <- 1
  
  while (suscept$R[t_1] < N) {
    suscept$S[t_1 + 1] <- rbinom(1, suscept$S[t_1], (1 - alpha)^(suscept$I[t_1]))
    suscept$R[t_1 + 1] <- suscept$R[t_1] + rbinom(1, suscept$I[t_1], beta)
    suscept$I[t_1 + 1] <- N - suscept$S[t_1 + 1] - suscept$R[t_1 + 1]
    torf <- suscept$S + suscept$I + suscept$R == N
    t_1 <- t_1 + 1
  }
  return(list(sir = suscept, tf = torf))
}

ans <- SIR(alpha,beta,N)
ans1 <- data.frame(ans[1])
ans1$Num <- 1:length(ans1$sir.S)
ggplot(ans1, aes(x = Num)) + 
  geom_line(ans1, mapping = aes(y = sir.S, color = "S")) +
  geom_line(ans1, mapping = aes(y = sir.I, color = "I")) +
  geom_line(ans1, mapping = aes(y = sir.R, color = "R"))

ans2 <- SIR(alpha2,beta2,N)
ans12 <- data.frame(ans2[1])
ans12$Num <- 1:length(ans12$sir.S)
ggplot(ans12, aes(x = Num)) + 
  geom_line(ans12, mapping = aes(y = sir.S, color = "S")) +
  geom_line(ans12, mapping = aes(y = sir.I, color = "I")) +
  geom_line(ans12, mapping = aes(y = sir.R, color = "R"))

ans3 <- SIR(alpha3,beta3,N)
ans13 <- data.frame(ans3[1])
ans13$Num <- 1:length(ans13$sir.S)
ggplot(ans13, aes(x = Num)) + 
  geom_line(ans13, mapping = aes(y = sir.S, color = "S")) +
  geom_line(ans13, mapping = aes(y = sir.I, color = "I")) +
  geom_line(ans13, mapping = aes(y = sir.R, color = "R"))

ans4 <- SIR(alpha4,beta4,N)
ans14 <- data.frame(ans4[1])
ans14$Num <- 1:length(ans14$sir.S)
ggplot(ans14, aes(x = Num)) + 
  geom_line(ans14, mapping = aes(y = sir.S, color = "S")) +
  geom_line(ans14, mapping = aes(y = sir.I, color = "I")) +
  geom_line(ans14, mapping = aes(y = sir.R, color = "R"))

# ************************************************************************

SIRD <- function(alpha,beta,d,rs,N) { # (alpha,beta,d,rs,N)
  
  susceptR <- list() # list with SIRRS
  susceptR$S[1] <- N - 1
  susceptR$I[1] <- 1
  susceptR$R[1] <- 0
  susceptR$RS[1] <- 0
  t_1R <- 1
  counts <- 0
  while (susceptR$R[t_1R] < N) {
    susceptR$S[t_1R + 1] <- rbinom(1, susceptR$S[t_1R], (1 - alpha)^(susceptR$I[t_1R]))
    susceptR$R[t_1R + 1] <- susceptR$R[t_1R] + rbinom(1, susceptR$I[t_1R], beta)
    susceptR$RS[t_1R + 1] <- rbinom(1, susceptR$R[t_1R], rs)
    susceptR$I[t_1R + 1] <- N - susceptR$S[t_1R + 1] - susceptR$R[t_1R + 1]
    torfR <- susceptR$S + susceptR$I + susceptR$R == N 
    t_1R <- t_1R + 1
    # counts <- counts+1
    # print(counts)
  }
  
  susceptD <- list() # list with SID
  susceptD$S[1] <- N - 1
  susceptD$I[1] <- 1
  susceptD$D[1] <- 0
  t_1 <- 1
  
  while (susceptD$D[t_1] < N) {
    susceptD$S[t_1 + 1] <- rbinom(1, susceptD$S[t_1], (1 - alpha)^(susceptD$I[t_1]))
    susceptD$D[t_1 + 1] <- susceptD$D[t_1] + rbinom(1, susceptD$I[t_1], d)
    susceptD$I[t_1 + 1] <- N - susceptD$S[t_1 + 1] - susceptD$D[t_1 + 1]
    torfD <- susceptD$S + susceptD$I + susceptD$D == N 
    t_1 <- t_1 + 1
    # counts <- counts+1
    # print(counts)
  }
  return(list(sir = susceptR, sid = susceptD, t_R = t_1R, t_D = t_1))
}


# Four scenarios:

# very mild virus with population of 1000000
# # mildly contagious with mild outcomes
N <- 1000000
# # Prob S to I
alpha <- 0.0001
# # Prob I to R 
beta <- 0.07
# # Prob I to D
d <- 0.0009
# # Prob R to S
rs <- 0.05
mild <- SIRD(alpha,beta,d,rs,N)

# very contagious but not deadly virus with population of 1000000
N <- 1000000
# # Prob S to I
alpha <- 0.43
# # Prob I to R 
beta <- 0.1
# # Prob I to D
d <- 0.0004
# # Prob R to S
rs <- 0.09
onlycontagious <- SIRD(alpha,beta,d,rs,N)

# mildly contagious but deadly virus with population of 1000000
N <- 1000000
# # Prob S to I
alpha <- 0.0002
# # Prob I to R 
beta <- 0.0007
# # Prob I to D
d <- 0.01
# # Prob R to S
rs <- 0.0001
dangermild <- SIRD(alpha,beta,d,rs,N)

# very contagious and deadly virus with population of 1000000
N <- 1000000
# # Prob S to I
alpha <- 0.2
# # Prob I to R 
beta <- 0.00055
# # Prob I to D
d <- 0.25
# # Prob R to S
rs <- 0.22
danger <- SIRD(alpha,beta,d,rs,N)


# Plots of the four scenarios 
library(ggplot2)

# mild scenario
mildR <- data.frame(mild[1])
mildR$Num <- 1:length(mildR$sir.S)
ggplot(mildR, aes(x = Num)) + 
  geom_line(mildR, mapping = aes(y = sir.S, color = "S")) +
  geom_line(mildR, mapping = aes(y = sir.I, color = "I")) +
  geom_line(mildR, mapping = aes(y = sir.R, color = "R"))

ggplot(mildR, aes(x = Num)) + 
  geom_line(mildR, mapping = aes(y = sir.R, color = "R")) +
  geom_line(mildR, mapping = aes(y = sir.RS, color = "RS"))

mildD <- data.frame(mild[2])
mildD$Num <- 1:length(mildD$sid.S)
ggplot(mildD, aes(x = Num)) + 
  geom_line(mildD, mapping = aes(y = sid.S, color = "S")) +
  geom_line(mildD, mapping = aes(y = sid.I, color = "I")) +
  geom_line(mildD, mapping = aes(y = sid.D, color = "D"))


# onlycontagious scenario
ocontagR <- data.frame(onlycontagious[1])
ocontagR$Num <- 1:length(ocontagR$sir.S)
ggplot(ocontagR, aes(x = Num)) + 
  geom_line(ocontagR, mapping = aes(y = sir.S, color = "S")) +
  geom_line(ocontagR, mapping = aes(y = sir.I, color = "I")) +
  geom_line(ocontagR, mapping = aes(y = sir.R, color = "R"))

ggplot(ocontagR, aes(x = Num)) + 
  geom_line(ocontagR, mapping = aes(y = sir.R, color = "R")) +
  geom_line(ocontagR, mapping = aes(y = sir.RS, color = "RS"))

ocontagD <- data.frame(onlycontagious[2])
ocontagD$Num <- 1:length(ocontagD$sid.S)
ggplot(ocontagD, aes(x = Num)) + 
  geom_line(ocontagD, mapping = aes(y = sid.S, color = "S")) +
  geom_line(ocontagD, mapping = aes(y = sid.I, color = "I")) +
  geom_line(ocontagD, mapping = aes(y = sid.D, color = "D"))


# dangermild scenario
dmR <- data.frame(dangermild[1])
dmR$Num <- 1:length(dmR$sir.S)
ggplot(dmR, aes(x = Num)) + 
  geom_line(dmR, mapping = aes(y = sir.S, color = "S")) +
  geom_line(dmR, mapping = aes(y = sir.I, color = "I")) +
  geom_line(dmR, mapping = aes(y = sir.R, color = "R"))

ggplot(dmR, aes(x = Num)) + 
  geom_line(dmR, mapping = aes(y = sir.R, color = "R")) +
  geom_line(dmR, mapping = aes(y = sir.RS, color = "RS"))

dmD <- data.frame(dangermild[2])
dmD$Num <- 1:length(dmD$sid.S)
ggplot(dmD, aes(x = Num)) + 
  geom_line(dmD, mapping = aes(y = sid.S, color = "S")) +
  geom_line(dmD, mapping = aes(y = sid.I, color = "I")) +
  geom_line(dmD, mapping = aes(y = sid.D, color = "D"))


# danger scenario
dangerR <- data.frame(danger[1])
dangerR$Num <- 1:length(dangerR$sir.S)
ggplot(dangerR, aes(x = Num)) + 
  geom_line(dangerR, mapping = aes(y = sir.S, color = "S")) +
  geom_line(dangerR, mapping = aes(y = sir.I, color = "I")) +
  geom_line(dangerR, mapping = aes(y = sir.R, color = "R"))

ggplot(dangerR, aes(x = Num)) + 
  geom_line(dangerR, mapping = aes(y = sir.R, color = "R")) +
  geom_line(dangerR, mapping = aes(y = sir.RS, color = "RS"))

dangerD <- data.frame(danger[2])
dangerD$Num <- 1:length(dangerD$sid.S)
ggplot(dangerD, aes(x = Num)) + 
  geom_line(dangerD, mapping = aes(y = sid.S, color = "S")) +
  geom_line(dangerD, mapping = aes(y = sid.I, color = "I")) +
  geom_line(dangerD, mapping = aes(y = sid.D, color = "D"))

