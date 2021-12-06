# Latest version but need to check the sim project Rscript that is connected to github

# ************************************************************************
alpha <- 0.08
beta <- 0.03
alpha2 <- 0.2
beta2 <- 0.5
alpha3 <- 0.7
beta3 <- 0.01
alpha4 <- 0.02
beta4 <- 0.8

SIRDS <- function(alpha,beta,N) { # (alpha,beta,d,rs,N)
  
  suscept <- list() # list with SIRDS
  suscept$S[1] <- 9999
  suscept$I[1] <- 1
  suscept$R[1] <- 0
  # suscept$D[1] <- 0
  # suscept$RS[1] <- 0
  # suscept$moved[1] <- 0
  # probrs <- 0
  t_1 <- 1
  
  while (suscept$R[t_1] < N) {
    #while (TRUE) { # only when running a function i.e. while true runs until return a value i.e. in a function
    suscept$S[t_1 + 1] <- rbinom(1, suscept$S[t_1], (1 - alpha)^(suscept$I[t_1]))
    suscept$R[t_1 + 1] <- suscept$R[t_1] + rbinom(1, suscept$I[t_1], beta)
    suscept$I[t_1 + 1] <- N - suscept$S[t_1 + 1] - suscept$R[t_1 + 1] # - suscept$D[t_1 + 1] #suscept$I[t_1] + rbinom(1, suscept$S[t_1], alpha)
    # suscept$D[t_1 + 1] <- suscept$D[t_1] + rbinom(1, suscept$I[t_1], d)
    # print(suscept$D[t_1])
    torf <- suscept$S + suscept$I + suscept$R == N
    t_1 <- t_1 + 1
  }
  return(list(sir = suscept, tf = torf))
}
ans <- SIRDS(alpha,beta,N)
ans1 <- data.frame(ans[1])
ans1$Num <- 1:length(ans1$sir.S)
ggplot(ans1, aes(x = Num)) + 
  geom_line(ans1, mapping = aes(y = sir.S, color = "S")) +
  geom_line(ans1, mapping = aes(y = sir.I, color = "I")) +
  geom_line(ans1, mapping = aes(y = sir.R, color = "R"))

ans2 <- SIRDS(alpha2,beta2,N)
ans12 <- data.frame(ans2[1])
ans12$Num <- 1:length(ans12$sir.S)
ggplot(ans12, aes(x = Num)) + 
  geom_line(ans12, mapping = aes(y = sir.S, color = "S")) +
  geom_line(ans12, mapping = aes(y = sir.I, color = "I")) +
  geom_line(ans12, mapping = aes(y = sir.R, color = "R"))

ans3 <- SIRDS(alpha3,beta3,N)
ans13 <- data.frame(ans3[1])
ans13$Num <- 1:length(ans13$sir.S)
ggplot(ans13, aes(x = Num)) + 
  geom_line(ans13, mapping = aes(y = sir.S, color = "S")) +
  geom_line(ans13, mapping = aes(y = sir.I, color = "I")) +
  geom_line(ans13, mapping = aes(y = sir.R, color = "R"))

ans4 <- SIRDS(alpha4,beta4,N)
ans14 <- data.frame(ans4[1])
ans14$Num <- 1:length(ans14$sir.S)
ggplot(ans14, aes(x = Num)) + 
  geom_line(ans14, mapping = aes(y = sir.S, color = "S")) +
  geom_line(ans14, mapping = aes(y = sir.I, color = "I")) +
  geom_line(ans14, mapping = aes(y = sir.R, color = "R"))
# ************************************************************************

SIRD <- function(alpha,beta,d,rs,N) { # (alpha,beta,d,rs,N)
  
  susceptR <- list() # list with SIR
  susceptR$S[1] <- N - 1
  susceptR$I[1] <- 1
  susceptR$R[1] <- 0
  susceptR$RS[1] <- 0
  t_1R <- 1
  counts <- 0
  while (susceptR$R[t_1R] < N) {
    #while (TRUE) { # only when running a function i.e. while true runs until return a value i.e. in a function
    susceptR$S[t_1R + 1] <- rbinom(1, susceptR$S[t_1R], (1 - alpha)^(susceptR$I[t_1R]))
    susceptR$R[t_1R + 1] <- susceptR$R[t_1R] + rbinom(1, susceptR$I[t_1R], beta)
    susceptR$RS[t_1R + 1] <- rbinom(1, susceptR$R[t_1R], rs)
    susceptR$I[t_1R + 1] <- N - susceptR$S[t_1R + 1] - susceptR$R[t_1R + 1]
    #susceptR$RS[t_1R + 1] <- susceptR$S[t_1R] + rbinom(1, susceptR$R[t_1R], rs)
    torfR <- susceptR$S + susceptR$I + susceptR$R == N 
    t_1R <- t_1R + 1
    counts <- counts+1
    print(counts)
  }
  
  susceptD <- list() # list with SIR
  susceptD$S[1] <- N - 1
  susceptD$I[1] <- 1
  susceptD$D[1] <- 0
  t_1 <- 1
  
  while (susceptD$D[t_1] < N) {
    #while (TRUE) { # only when running a function i.e. while true runs until return a value i.e. in a function
    susceptD$S[t_1 + 1] <- rbinom(1, susceptD$S[t_1], (1 - alpha)^(susceptD$I[t_1]))
    susceptD$D[t_1 + 1] <- susceptD$D[t_1] + rbinom(1, susceptD$I[t_1], d)
    susceptD$I[t_1 + 1] <- N - susceptD$S[t_1 + 1] - susceptD$D[t_1 + 1]
    torfD <- susceptD$S + susceptD$I + susceptD$D == N 
    t_1 <- t_1 + 1
    counts <- counts+1
    print(counts)
  }
  return(list(sir = susceptR, sid = susceptD, t_R = t_1R, t_D = t_1)) #, tfR = torfR, tfD = torfD))
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
dangermild <- SIRD(alpha,beta,d,rs,N) # mclapply(1:2, FUN = SIRD(alpha,beta,d,rs,N), mc.cores = 2) #SIRD(alpha,beta,d,rs,N)

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
danger <- SIRD(alpha,beta,d,rs,N) # mclapply(1:1000000, FUN = SIRD(alpha,beta,d,rs,N), mc.cores = 2)

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




for (i in ans1[1]) {
  plot(i$S,type = "l",col = "green")
  points(i$I,type = "l", col = "red")
  points(i$R,type = "l", col = "blue")
  #points(i$D,type = "l", col = "black")
  #points(i$RS,type = "l", col = "pink")
}
for (i in ans1[2]) {
  plot(i$S,type = "l",col = "green")
  points(i$I,type = "l", col = "red")
  points(i$D,type = "l", col = "blue")
  #points(i$D,type = "l", col = "black")
  #points(i$RS,type = "l", col = "pink")
}

ans11R <- data.frame(ans1[1])
ans11R$Num <- 1:length(ans11R$sir.S)
plotR <- ggplot(ans11R, aes(x = Num)) + 
  geom_line(ans11R, mapping = aes(y = sir.S, color = "S")) +
  geom_line(ans11R, mapping = aes(y = sir.I, color = "I")) +
  geom_line(ans11R, mapping = aes(y = sir.R, color = "R"))

ans11D <- data.frame(ans1[2])
ans11D$Num <- 1:length(ans11D$sid.S)
plotD <- ggplot(ans11D, aes(x = Num)) + 
  geom_line(ans11D, mapping = aes(y = sid.S, color = "S")) +
  geom_line(ans11D, mapping = aes(y = sid.I, color = "I")) +
  geom_line(ans11D, mapping = aes(y = sid.D, color = "D"))


ans2 <- SIRDS(alpha2,beta2,d2,rs1,N)
ans3 <- SIRDS(alpha3,beta3,d3,rs1,N)
ans4 <- SIRDS(alpha4,beta4,d4,rs1,N)
lens <- 0:length(ans1$S)
library(ggplot2)


storean <- rep(0,2000)
for (i in 1:4) {
  ans <- SIRDS(alpha[i],beta[i],d[i],rs[i],N)
  storean[i] <- ans
}
storeans <- c()
for (i in alpha) {
  for (j in beta) {
    for (m in d) {
      for (a in rs) {
        ans <- SIRDS(i,j,m,a,N)
        storeans <- c(ans,storeans)
      }
    }
  }
}
oddnum <- seq(1,512,2)
sirdss <- c()
for (i in oddnum) {
  sirdsstore <- storeans[i]
  sirdss <- c(sirdsstore,sirdss)
  
}

for (i in sirdss) {
  plot(i$S,type = "l",col = "green")
  points(i$I,type = "l", col = "red")
  points(i$R,type = "l", col = "blue")
  points(i$D,type = "l", col = "black")
  points(i$RS,type = "l", col = "pink")
}


sirdsfxn <- SIRDS(alpha1,beta1,d1,rs1,N1)

plot(sirdsfxn$sirds$S,type = "l",col = "green")
points(sirdsfxn$sirds$I,type = "l", col = "red")
points(sirdsfxn$sirds$R,type = "l", col = "blue")
points(sirdsfxn$sirds$D,type = "l", col = "black")
points(sirdsfxn$sirds$RS,type = "l", col = "pink")





# for (i in suscept$R) {
#   print(i)
#   suscept$D[i] <- rbinom(1, suscept$R[i + 1], d)
# }



if (suscept$R[t_1] == (N + 1)) {
  suscept$S[(length(suscept$S))] <- suscept$R[(length(suscept$R))]
  suscept$R[(length(suscept$R))] <- 0
}
# t_1 <- t+1
suscept$I[(length(suscept$I))] <- 1

while (suscept$R[(length(suscept$R))] < (N + 1)) {
  suscept$S[t_1 + 1] <- rbinom(1, suscept$S[t_1], (1 - alpha) ^ (suscept$I[t_1]))
  
  suscept$R[t_1 + 1] <- suscept$R[t_1] + rbinom(1, suscept$I[t_1], beta)
  
  suscept$I[t_1 + 1] <- N + 1 - suscept$S[t_1 + 1] - suscept$R[t_1 + 1]
  
  t_1 <- t_1 + 1
}

# suscept$S[(length(suscept$S))] <- suscept$R[(length(suscept$R))]
suscept
t
