# pop size
N <- 10000

# Prob S to I
alpha <- 0.05

# Prob I to R
beta <- 0.004

# Prob R to D
d <- 0.0005

suscept <- list() # list with SIRD
suscept$S[1] <- N
suscept$I[1] <- 1
suscept$R[1] <- 0
suscept$D[1] <- 0

t_1 <- 1

while (suscept$R[t_1] < N) {
  #while (TRUE) { # only when running a function i.e. while true runs until return a value i.e. in a function
  suscept$S[t_1 + 1] <- rbinom(1, suscept$S[t_1], (1 - alpha)^(suscept$I[t_1]))
  suscept$R[t_1 + 1] <- suscept$R[t_1] + rbinom(1, suscept$I[t_1], beta)
  suscept$I[t_1 + 1] <- N - suscept$S[t_1 + 1] - suscept$R[t_1 + 1]
  suscept$D[t_1 + 1] <- suscept$D[t_1] + rbinom(1, suscept$R[t_1], d)
  
  t_1 <- t_1 + 1
  
  # # part where I am trying to restore/add all the recovered (R) to susceptible pop (S)
  # if (suscept$S == 0) {
  #   while (TRUE) {
  #     suscept$S[t_1] <- suscept$R[t_1]
  #     suscept$R[t_1] <- 0
}
#}
#   }
# }

plot(suscept$S,type = "l",col = "green")
points(suscept$I,type = "l", col = "red")
points(suscept$R,type = "l", col = "blue")
points(suscept$D,type = "l", col = "black")





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
