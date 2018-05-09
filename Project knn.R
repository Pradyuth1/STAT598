library(plyr)
library(class)
library(tidyverse)
library(ggvis)
Iris <- read.csv("C:\\Users\\pradyuth\\Documents\\R\\STAT 598 project\\Iris.csv")

#Sampling the data
set.seed(100)
Iris <- Iris[sample(nrow(Iris)) , ]

Split_data <- function(a){
  u <- nrow(Iris)
  Iris_training <- Iris[1:(a*u), ]
  Iris_test <- Iris[((a*u)+1):(u+1), ]
  return(list(Iris_training, Iris_test))
}

set_1 <- na.omit(Split_data(0.85))
set_2 <- na.omit(Split_data(0.75))
set_3 <- na.omit(Split_data(0.65))
set_4 <- na.omit(Split_data(0.95))

nrow(Iris)

Accuracy <- function(dataset, k){
  
  euc_dist<- function(p,q){
    len<- sqrt(sum((p-q)^2))
    return(len)
  }
  
  #Finding the distances between various columns:
  dist <- function(dataset){
    distance <- matrix(0, nrow = nrow(dataset[[1]]), ncol = nrow(dataset[[2]]))
    for(i in 1:nrow(dataset[[2]])){
      for(j in 1:nrow(dataset[[1]])){
        distance[j, i] <- euc_dist(dataset[[1]][j, 2:5], dataset[[2]][i, 2:5])
      }
    }
    return(distance)
  }
  
  distance <- dist(dataset)
  
  #Find k least distances
  least_indices <- function(dataset, k){
    indices <- list()
    for(i in 1:nrow(dataset[[2]])){
      indices[i][[1]] <- sort(distance[, i], decreasing = F, index.return = T)[[2]][1:k]
    }
    return(indices)
  }
  
  #Finding the majority of k nearest neighbours
  pred_res_var <- function(dataset, k){
    a <- list()
    b <- list()
    for(i in 1:nrow(dataset[[2]])){
      a[i][[1]] <- max(table(dataset[[1]][least_indices(dataset, k)[[i]], 6]))
      b[i][[1]] <- which(table(dataset[[1]][least_indices(dataset, k)[[i]], 6]) == a[i][[1]])
    }
    return(b)
  }
  
  pred_resp_var_values <- pred_res_var(dataset, k)
  #Accuracy is given by:
  accuracy_value <- mean(rownames(as.matrix(unlist(pred_resp_var_values))) == dataset[[2]][,6])
  
  return(accuracy_value)

}

#Accuracy of the model using knn function
Accuracy_knn <- function(dataset, k){
  set_pred <- knn(train = dataset[[1]][,-c(1,6)], test = dataset[[2]][,-c(1,6)], cl = dataset[[1]]$Species, k = k)
  return(mean(set_pred == dataset[[2]][,6]))
}

#Accuracy values are tabulated in form of matrix
values <- function(func, dataset, k){
  value <- matrix(rep(0), nrow = length(dataset), ncol = length(k), byrow = TRUE)
  for(i in 1:length(dataset)){
    for(j in 1:length(k)){
      value[i, j] <- func(Split_data(dataset[i]), k[j])
    }
  }
  colnames(value) <- paste("k-value",k)
  rownames(value) <- paste("Split-ratio",dataset)
  return(value)
}

#Accuracy values for written function
values_accuracy <- values(Accuracy, c(0.65,0.75,0.85,0.95), c(4, 7, 9, 11, 13, 15))
values_accuracy

#Accuracy values for knn inbuilt function
values_accuracy_knn <- values(Accuracy_knn, c(0.65,0.75,0.85,0.95), c(4, 7, 9, 11, 13, 15))
values_accuracy_knn

#Error values for a particular function, various sets of data and k
error_val <- function(func, dataset, k){
  values <- values(func, dataset, k)
  error_values <- (1 - values)
  error_values <- as.data.frame(t(error_values))
  a<- c(4,7,9,11,13,15)
  error_values <- cbind(a,error_values)
  return(error_values)
}

#Error values for accuracy function
error_val(Accuracy, c(0.65,0.75,0.85,0.95), c(4, 7, 9, 11, 13, 15))

#Error values for accuracy knn function
error_val(Accuracy_knn, c(0.65,0.75,0.85,0.95), c(4, 7, 9, 11, 13, 15))











