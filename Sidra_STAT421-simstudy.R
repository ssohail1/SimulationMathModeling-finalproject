# pop size
N <- 10000

# Prob S to I
alpha <- 0.0002

# Prob I to R #infects quickly
beta <- 0.03

# Prob R to D # is not as deadly
d <- 0.001

# Prob R to S # can be quickly infected again
rs <- 0.09


SIRDS <- function(StoIprob,ItoRprob,RtoDprob,RtoSprob,Npopul) {
  
suscept <- list() # list with SIRDS
suscept$S[1] <- N
suscept$I[1] <- 1
suscept$R[1] <- 0
suscept$D[1] <- 0
suscept$RS[1] <- 0

t_1 <- 1

while (suscept$R[t_1] < N) {
  #while (TRUE) { # only when running a function i.e. while true runs until return a value i.e. in a function
  suscept$S[t_1 + 1] <- rbinom(1, suscept$S[t_1], (1 - alpha)^(suscept$I[t_1]))
  suscept$R[t_1 + 1] <- suscept$R[t_1] + rbinom(1, suscept$I[t_1], beta)
  suscept$I[t_1 + 1] <- N - suscept$S[t_1 + 1] - suscept$R[t_1 + 1]
  suscept$D[t_1 + 1] <- suscept$D[t_1] + rbinom(1, suscept$R[t_1], d)
  suscept$RS[t_1 + 1] <- suscept$S[t_1] + rbinom(1, suscept$R[t_1], rs)
  t_1 <- t_1 + 1
  
  # # part where I am trying to restore/add all the recovered (R) to susceptible pop (S)
  # if (suscept$S == 0) {
  #   while (TRUE) {
  #     suscept$S[t_1] <- suscept$R[t_1]
  #     suscept$R[t_1] <- 0
}
return(list(sirds = suscept, t_ = length(suscept$S), t_1))
}
#   }
# }

sirdsfxn <- SIRDS(alpha,beta,d,rs,N)

plot(sirdsfxn$sirds$S,type = "l",col = "green")
points(sirdsfxn$sirds$I,type = "l", col = "red")
points(sirdsfxn$sirds$R,type = "l", col = "blue")
points(sirdsfxn$sirds$D,type = "l", col = "black")
points(sirdsfxn$sirds$RS,type = "l", col = "pink")
