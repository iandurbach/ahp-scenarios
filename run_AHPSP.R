library(tidyverse)

source("generate_baseline_evaluations.R")
source("generate_state_evaluations.R")
source("get_AHP_scores.R")

Ni <- 9
J <- 5
pDbn <- 1
skewbase <- 0
skewrange <- 0.01
stddevbase <- 0.1
stddevrange <- 0.05
nMainScens <- 10

p <- sample(c(40,40,40,40,40,40,40,40,40,40),nMainScens,replace=FALSE)
p <- sample(c(60,60,60,60,40,40,20,20,20,20),nMainScens,replace=FALSE)
p <- sample(c(103,103,52,52,26,26,13,13,6,6),nMainScens,replace=FALSE)

z <- generate_state_evaluations(9,5,p,0,0.001,0.01,0.04)

v_all <- z %>% group_by(i,j) %>% 
  summarize(v = mean(v)) %>% 
  ungroup() %>%
  spread(key = j, value = v) %>%
  select(-i) %>% as.matrix()

# generate w as U(0.1,0.9) so max/min < 9
w <- matrix(runif(J,0.1,0.9), J, 1)
# compute PCM
wPCM <- w %*% (1/t(w))  
# weights
w <- Re(eigen(wPCM)$vectors[,1])
w <- w / sum(w)

## compute global scores
V <- v_all %*% w
print(V)
