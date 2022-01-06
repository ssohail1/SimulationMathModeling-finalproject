# Baseline model
## SIR model with four different scenarios
# Population
N <- 1000000
# Not Contagious and Fatal
# low infection rate and low recovery
# # Prob S to I
alpha <- 0.0006
# # Prob I to R
beta <- 0.0005

# Contagious and Not Fatal 
# high infection rate and high recovery
# # Prob S to I
alpha2 <- 0.2
# # Prob I to R
beta2 <- 0.5

# Contagious and Fatal
# high infection rate and low recovery
# # Prob S to I
alpha3 <- 0.7
# # Prob I to R
beta3 <- 0.01

# Mild - Not Contagious and Not Fatal
# low infection rate and high recovery
# # Prob S to I
alpha4 <- 0.02
# # Prob I to R
beta4 <- 0.8

# SIR function
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

library(ggplot2)

# Not Contagious and Fatal
set.seed(1234)
ans <- SIR(alpha,beta,N)
ans1 <- data.frame(ans[1])
ans1$Num <- 1:length(ans1$sir.S)
ggplot(ans1, aes(x = Num),) + 
  geom_line(ans1, mapping = aes(y = sir.S, color = "S")) +
  geom_line(ans1, mapping = aes(y = sir.I, color = "I")) +
  geom_line(ans1, mapping = aes(y = sir.R, color = "R")) +
  labs(x = "Number of Days", y = "Number of People", title = "Fig.1a: SIR Model - Not Contagious - Fatal") +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12), axis.title.x = element_text(size = 12), 
        axis.text.x = element_text(size = 12), axis.title.y = element_text(size = 12), 
        axis.text.y = element_text(size = 12), title = element_text(size = 12))

# Contagious and Not Fatal 
set.seed(1234)
ans2 <- SIR(alpha2,beta2,N)
ans12 <- data.frame(ans2[1])
ans12$Num <- 1:length(ans12$sir.S)
ggplot(ans12, aes(x = Num)) + 
  geom_line(ans12, mapping = aes(y = sir.S, color = "S")) +
  geom_line(ans12, mapping = aes(y = sir.I, color = "I")) +
  geom_line(ans12, mapping = aes(y = sir.R, color = "R")) +
  labs(x = "Number of Days", y = "Number of People", title = "Fig.1b: SIR Model - Contagious - Not Fatal") +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12), axis.title.x = element_text(size = 12), 
        axis.text.x = element_text(size = 12), axis.title.y = element_text(size = 12), 
        axis.text.y = element_text(size = 12), title = element_text(size = 12))

# Contagious and Fatal
set.seed(1234)
ans3 <- SIR(alpha3,beta3,N)
ans13 <- data.frame(ans3[1])
ans13$Num <- 1:length(ans13$sir.S)
ggplot(ans13, aes(x = Num)) + 
  geom_line(ans13, mapping = aes(y = sir.S, color = "S")) +
  geom_line(ans13, mapping = aes(y = sir.I, color = "I")) +
  geom_line(ans13, mapping = aes(y = sir.R, color = "R")) +
  labs(x = "Number of Days", y = "Number of People", title = "Fig.1c: SIR Model - Contagious - Fatal") +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12), 
        axis.title.x = element_text(size = 12), axis.text.x = element_text(size = 12), 
        axis.title.y = element_text(size = 12), axis.text.y = element_text(size = 12), title = element_text(size = 12))

# Mild - Not Contagious and Not Fatal
set.seed(1234)
ans4 <- SIR(alpha4,beta4,N)
ans14 <- data.frame(ans4[1])
ans14$Num <- 1:length(ans14$sir.S)
ggplot(ans14, aes(x = Num)) + 
  geom_line(ans14, mapping = aes(y = sir.S, color = "S")) +
  geom_line(ans14, mapping = aes(y = sir.I, color = "I")) +
  geom_line(ans14, mapping = aes(y = sir.R, color = "R")) +
  labs(x = "Number of Days", y = "Number of People", title = "Fig.1d: SIR Model - Mild") +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12), 
        axis.title.x = element_text(size = 12), axis.text.x = element_text(size = 12), axis.title.y = element_text(size = 12), 
        axis.text.y = element_text(size = 12), title = element_text(size = 12))


# SIRRS and SID models
# SIRRS and SID
# SIRD function
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
    
  }
  return(list(sir = susceptR, sid = susceptD, t_R = t_1R, t_D = t_1))
}

# Four scenarios:

# very mild disease with population of 1000000
# # mildly contagious with mild outcomes
library(ggplot2)

N <- 1000000
# # Prob S to I
alpha <- 0.0001
# # Prob I to R 
beta <- 0.09
# # Prob I to D
d <- 0.0009
# # Prob R to S
rs <- 0.05

set.seed(1234)
mild <- SIRD(alpha,beta,d,rs,N)

# very contagious with mild outcomes with population of 1000000
N <- 1000000
# # Prob S to I
alpha <- 0.43
# # Prob I to R 
beta <- 0.1
# # Prob I to D
d <- 0.0004
# # Prob R to S
rs <- 0.09

set.seed(1234)
onlycontagious <- SIRD(alpha,beta,d,rs,N)

# mildly contagious with fatal outcomes with population of 1000000
N <- 1000000
# # Prob S to I
alpha <- 0.0002
# # Prob I to R 
beta <- 0.0007
# # Prob I to D
d <- 0.01
# # Prob R to S
rs <- 0.0001

set.seed(1234)
dangermild <- SIRD(alpha,beta,d,rs,N)

# very contagious with fatal outcomes with population of 1000000
N <- 1000000
# # Prob S to I
alpha <- 0.2
# # Prob I to R 
beta <- 0.00055
# # Prob I to D
d <- 0.25
# # Prob R to S
rs <- 0.22

set.seed(1234)
danger <- SIRD(alpha,beta,d,rs,N)


# Plots of the four scenarios 
library(ggplot2)

# mild scenario
# SIR
mildR <- data.frame(mild[1])
mildR$Num <- 1:length(mildR$sir.S)
ggplot(mildR, aes(x = Num)) + 
  geom_line(mildR, mapping = aes(y = sir.S, color = "S")) +
  geom_line(mildR, mapping = aes(y = sir.I, color = "I")) +
  geom_line(mildR, mapping = aes(y = sir.R, color = "R")) +
  labs(x = "Number of Days", y = "Number of People", title = "Fig.2a: SIRRS Model - Mild") +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12),
        axis.title.x = element_text(size = 12), axis.text.x = element_text(size = 12), 
        axis.title.y = element_text(size = 12), axis.text.y = element_text(size = 12), title = element_text(size = 12))

# RRS
ggplot(mildR, aes(x = Num)) + 
  geom_line(mildR, mapping = aes(y = sir.R, color = "R")) +
  geom_line(mildR, mapping = aes(y = sir.RS, color = "RS")) +
  labs(x = "Number of Days", y = "Number of People", title = " Fig.2b: RRS Model - Mild") +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12), 
        axis.title.x = element_text(size = 12), axis.text.x = element_text(size = 12), 
        axis.title.y = element_text(size = 12), axis.text.y = element_text(size = 12), title = element_text(size = 12))

# SID
mildD <- data.frame(mild[2])
mildD$Num <- 1:length(mildD$sid.S)
ggplot(mildD, aes(x = Num)) + 
  geom_line(mildD, mapping = aes(y = sid.S, color = "S")) +
  geom_line(mildD, mapping = aes(y = sid.I, color = "I")) +
  geom_line(mildD, mapping = aes(y = sid.D, color = "D")) +
  labs(x = "Number of Days", y = "Number of People", title = "Fig.2c: SID Model - Mild") +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12), 
        axis.title.x = element_text(size = 12), axis.text.x = element_text(size = 12), 
        axis.title.y = element_text(size = 12), axis.text.y = element_text(size = 12), title = element_text(size = 12))


# onlycontagious scenario
# SIR
ocontagR <- data.frame(onlycontagious[1])
ocontagR$Num <- 1:length(ocontagR$sir.S)
ggplot(ocontagR, aes(x = Num)) + 
  geom_line(ocontagR, mapping = aes(y = sir.S, color = "S")) +
  geom_line(ocontagR, mapping = aes(y = sir.I, color = "I")) +
  geom_line(ocontagR, mapping = aes(y = sir.R, color = "R")) +
  labs(x = "Number of Days", y = "Number of People", title = "Fig.3a: SIRRS Model - Contagious - Not Fatal") +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12), 
        axis.title.x = element_text(size = 12), axis.text.x = element_text(size = 12), 
        axis.title.y = element_text(size = 12), axis.text.y = element_text(size = 12), title = element_text(size = 12))

# RRS
ggplot(ocontagR, aes(x = Num)) + 
  geom_line(ocontagR, mapping = aes(y = sir.R, color = "R")) +
  geom_line(ocontagR, mapping = aes(y = sir.RS, color = "RS")) +
  labs(x = "Number of Days", y = "Number of People", title = "Fig.3b: RRS Model - Contagious - Not Fatal") +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12), 
        axis.title.x = element_text(size = 12), axis.text.x = element_text(size = 12), 
        axis.title.y = element_text(size = 12), axis.text.y = element_text(size = 12), title = element_text(size = 12))

# SID
ocontagD <- data.frame(onlycontagious[2])
ocontagD$Num <- 1:length(ocontagD$sid.S)
ggplot(ocontagD, aes(x = Num)) + 
  geom_line(ocontagD, mapping = aes(y = sid.S, color = "S")) +
  geom_line(ocontagD, mapping = aes(y = sid.I, color = "I")) +
  geom_line(ocontagD, mapping = aes(y = sid.D, color = "D")) +
  labs(x = "Number of Days", y = "Number of People", title = "Fig.3c: SID Model - Contagious - Not Fatal") +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12), 
        axis.title.x = element_text(size = 12), axis.text.x = element_text(size = 12), 
        axis.title.y = element_text(size = 12), axis.text.y = element_text(size = 12), title = element_text(size = 12))


# dangermild scenario
# SIR
dmR <- data.frame(dangermild[1])
dmR$Num <- 1:length(dmR$sir.S)
ggplot(dmR, aes(x = Num)) + 
  geom_line(dmR, mapping = aes(y = sir.S, color = "S")) +
  geom_line(dmR, mapping = aes(y = sir.I, color = "I")) +
  geom_line(dmR, mapping = aes(y = sir.R, color = "R")) +
  labs(x = "Number of Days", y = "Number of People", title = "Fig.4a: SIRRS Model - Not Contagious - Fatal") +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12), 
        axis.title.x = element_text(size = 12), axis.text.x = element_text(size = 12), 
        axis.title.y = element_text(size = 12), axis.text.y = element_text(size = 12), title = element_text(size = 12))

# RRS
ggplot(dmR, aes(x = Num)) + 
  geom_line(dmR, mapping = aes(y = sir.R, color = "R")) +
  geom_line(dmR, mapping = aes(y = sir.RS, color = "RS")) +
  labs(x = "Number of Days", y = "Number of People", title = "Fig.4b: RRS Model - Not Contagious - Fatal") +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12), 
        axis.title.x = element_text(size = 12), axis.text.x = element_text(size = 12), 
        axis.title.y = element_text(size = 12), axis.text.y = element_text(size = 12), title = element_text(size = 12))

#SID
dmD <- data.frame(dangermild[2])
dmD$Num <- 1:length(dmD$sid.S)
ggplot(dmD, aes(x = Num)) + 
  geom_line(dmD, mapping = aes(y = sid.S, color = "S")) +
  geom_line(dmD, mapping = aes(y = sid.I, color = "I")) +
  geom_line(dmD, mapping = aes(y = sid.D, color = "D")) +
  labs(x = "Number of Days", y = "Number of People", title = "Fig.4c: SID Model - Not Contagious - Fatal") +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12), 
        axis.title.x = element_text(size = 12), axis.text.x = element_text(size = 12), 
        axis.title.y = element_text(size = 12), axis.text.y = element_text(size = 12), title = element_text(size = 12))


# danger scenario
# SIR
dangerR <- data.frame(danger[1])
dangerR$Num <- 1:length(dangerR$sir.S)
ggplot(dangerR, aes(x = Num)) + 
  geom_line(dangerR, mapping = aes(y = sir.S, color = "S")) +
  geom_line(dangerR, mapping = aes(y = sir.I, color = "I")) +
  geom_line(dangerR, mapping = aes(y = sir.R, color = "R")) +
  labs(x = "Number of Days", y = "Number of People", title = "Fig.5a: SIRRS Model - Contagious - Fatal") +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12), 
        axis.title.x = element_text(size = 12), axis.text.x = element_text(size = 12), 
        axis.title.y = element_text(size = 12), axis.text.y = element_text(size = 12), title = element_text(size = 12))

# RRS
ggplot(dangerR, aes(x = Num)) + 
  geom_line(dangerR, mapping = aes(y = sir.R, color = "R")) +
  geom_line(dangerR, mapping = aes(y = sir.RS, color = "RS")) +
  labs(x = "Number of Days", y = "Number of People", title = "Fig.5b: RRS Model - Contagious - Fatal") +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12), 
        axis.title.x = element_text(size = 12), axis.text.x = element_text(size = 12), 
        axis.title.y = element_text(size = 12), axis.text.y = element_text(size = 12), title = element_text(size = 12))

# SID
dangerD <- data.frame(danger[2])
dangerD$Num <- 1:length(dangerD$sid.S)
ggplot(dangerD, aes(x = Num)) + 
  geom_line(dangerD, mapping = aes(y = sid.S, color = "S")) +
  geom_line(dangerD, mapping = aes(y = sid.I, color = "I")) +
  geom_line(dangerD, mapping = aes(y = sid.D, color = "D")) +
  labs(x = "Number of Days", y = "Number of People", title = "Fig.5c: SID Model - Contagious - Fatal") +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12), 
        axis.title.x = element_text(size = 12), axis.text.x = element_text(size = 12), 
        axis.title.y = element_text(size = 12), axis.text.y = element_text(size = 12), title = element_text(size = 12))
