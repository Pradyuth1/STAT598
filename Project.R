Training_data <- function(a){
  Iris <- read.csv("C:\\Users\\pradyuth\\STAT 598 project\\Iris.csv")
  u <- nrow(Iris)
  Iris_training <- Iris[1:(a*u), ]
  Iris_test <- Iris[1:((1-a)*u), ]
  return(list(Iris_training, Iris_test))
}
