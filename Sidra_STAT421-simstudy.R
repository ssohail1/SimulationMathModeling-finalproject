# pop size
N1 <- 10000

# Prob S to I
alpha1 <- 0.0005

# Prob I to R #infects quickly
beta1 <- 0.07

# Prob I to D # is deadly
d1 <- 0.09

# Prob R to S # can be quickly susceptible again
rs1 <- 0.4

# probabilities as function of age group - going from 0-12, 12-25, 25-60, 60+
# # pop size
N <- 10000
# 
# # Prob S to I
alpha <- 0.05 #c(0.05,0.08,0.2,0.5)
alpha2 <- 0.08
alpha3 <- 0.2
alpha4 <- 0.5
# # Prob I to R 
beta <- 0.2 #c(0.2,0.3,0.07,0.03)
beta2 <- 0.3
beta3 <- 0.07
beta4 <- 0.03
# # Prob I to D
d <- 0.005 #c(0.005,0.004,0.02,0.08)
d2 <- 0.004
d3 <- 0.02
d4 <- 0.08
# # Prob R to S 
rs <- 0.04 #c(0.04,0.04,0.04,0.04)

SIRDS <- function(alpha,beta,d,rs,N) {
  
  suscept <- list() # list with SIRDS
  suscept$S[1] <- N
  suscept$I[1] <- 1
  suscept$R[1] <- 0
  suscept$D[1] <- 0
  # suscept$RS[1] <- 0
  # suscept$moved[1] <- 0
  # probrs <- 0
  t_1 <- 1
  
  while (suscept$R[t_1] < N) {
    #while (TRUE) { # only when running a function i.e. while true runs until return a value i.e. in a function
    suscept$S[t_1 + 1] <- rbinom(1, suscept$S[t_1], (1 - alpha)^(suscept$I[t_1])) #+ probrs #suscept$RS[t_1] - suscept$S ; suscept$moved[t_1]
    suscept$R[t_1 + 1] <- suscept$R[t_1] + rbinom(1, suscept$I[t_1], beta)
    suscept$I[t_1 + 1] <- N - suscept$S[t_1 + 1] - suscept$R[t_1 + 1]
    #suscept$R[t_1 + 1] <- suscept$R[t_1] + rbinom(1, suscept$I[t_1], beta) # - suscept$moved[t_1]
    suscept$D[t_1 + 1] <- suscept$D[t_1] + rbinom(1, suscept$I[t_1], d)
    # suscept$moved[t_1 + 1] <- rbinom(1, suscept$R[t_1], rs)
    # probrs <- rbinom(1, suscept$R[t_1], rs)
    #suscept$RS[t_1 + 1] <- suscept$S[t_1] + rbinom(1, suscept$R[t_1], rs) # + rbinom(1, suscept$R[t_1], rs) #suscept$moved[t_1]
    torf <- suscept$S + suscept$I + suscept$R + suscept$D == N #+ suscept$RS
    t_1 <- t_1 + 1
    
    # # part where I am trying to restore/add all the recovered (R) to susceptible pop (S)
    # if (suscept$S == 0) {
    #   while (TRUE) {
    #     suscept$S[t_1] <- suscept$R[t_1]
    #     suscept$R[t_1] <- 0
  }
  return(list(sirds = suscept, t_ = length(suscept$S, torf)))
}

ans1 <- SIRDS(alpha1,beta1,d1,rs1,N)
ans2 <- SIRDS(alpha2,beta2,d2,rs1,N)
ans3 <- SIRDS(alpha3,beta3,d3,rs1,N)
ans4 <- SIRDS(alpha4,beta4,d4,rs1,N)
anstab <- 
  library(ggplot2)
ggplot(data = )



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
