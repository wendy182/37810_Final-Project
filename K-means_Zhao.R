#input data for clustering, number of clusters,initial mean of each cluster
#output a vector indicating there clusters
library(rattle)
data(wine)
newdata <- wine[,-1]
k_means_cluster <- function(cluster_data,num_cluster){
  cluster_data <- as.matrix(cluster_data)
  num_points <- nrow(cluster_data)
  num_dim <- ncol(cluster_data)
  start_mean <- cluster_data[sample(1:num_points,num_cluster,replace = F),]#choose start mean
  new_cluster_label <- numeric(num_points)
  old_cluster_label <- rep(1,num_points)
  while (any(old_cluster_label != new_cluster_label)){
    old_cluster_label <- new_cluster_label
    distance <- matrix(0,num_points,num_cluster)
    for (j in 1:num_cluster){
      distance[,j] <- apply((cluster_data - matrix(start_mean[j,],nrow = num_points,ncol = num_dim,byrow = TRUE))^2,1,sum)
      #calculate distance to each cluster
    }
    new_cluster_label <- apply(distance,1,which.min)
    for (j in 1:num_cluster){
      start_mean[j,] <- apply(cluster_data[which(new_cluster_label==j),],2,mean)
      #recalculate cluster mean
    }
  }
  return(new_cluster_label)
}

label <- k_means_cluster(newdata,3)

library(fpc)

plotcluster(newdata,label)

plotcluster(newdata,wine$Type)

