library(plyr)
Iris <- read.csv("C:\\Users\\pradyuth\\STAT 598 project\\Iris.csv")

Split_data <- function(a){
  u <- nrow(Iris)
  Iris_training <- Iris[1:(a*u), ]
  Iris_test <- Iris[((a*u)+1):(u+1), ]
  return(list(Iris_training, Iris_test))
}

set_1 <- Split_data(0.85)
set_2 <- Split_data(0.70)

nrow(Iris)

euc_dist<- function(p,q){
  len<- sqrt(sum((p-q)^2))
  return(len)
}

distance <- matrix(0, nrow = nrow(set_1[[1]]), ncol = nrow(set_1[[2]]))

for(i in 1:nrow(set_1[[2]])){
  for(j in 1:nrow(set_1[[1]])){
    distance[j, i] <- euc_dist(set_1[[1]][j, 2:5], set_1[[2]][i, 2:5])
  }
}

#Find k least distances
indices <- sort(distance[, 2], decreasing = T, index.return = T)[[2]][1:4]

#Returning the categorical variable for indices obtained
set_1[[1]][indices, 6]











