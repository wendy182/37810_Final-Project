## Import Library and Data
library('rattle')
data(wine)
X <- wine[-1]

run_kmeans <- function(data,k){
  data <- as.matrix(data)
  num_points <- nrow(data)
  num_measurements <- ncol(data)
  start_mean <- data[sample(1:num_points, k, replace=F),]
  new_index <- rep(0,num_points)
  for(i in 1:5){
    distance <- matrix(0,num_points,k)
    for (j in 1:k){
      distance[,j] <- apply((data-matrix(start_mean[j,],nrow=num_points,ncol=num_measurements,byrow=T))^2,1,sum)
    }
    new_index <- apply(distance,1,which.min)
    for (n in 1:k){
      start_mean[j] <- apply(data[which(new_index == n),],2,mean)
    }
  }
  return (new_index)
}

run_kmeans(X,3)
