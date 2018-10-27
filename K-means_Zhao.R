#input data for clustering, number of clusters,initial mean of each cluster
#output a vector indicating there clusters

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
#calculate proportion matrix
matching_matrix <- function(old_type,new_type,num_cluster){
  proportion <- matrix(0,num_cluster,num_cluster)
  for(i in 1:num_cluster){
    index <- (new_type==i)
    for (j in 1:num_cluster){
      proportion[i,j] <- sum(old_type[index]==j)/sum(index)
    }
  }
  return(proportion)
}

#determine which cluster is linked to existing old cluster
new_cluster <- function(new_type,actual_type){
  new_index <- numeric(length(new_type))
   for (i in 1:length(actual_type)){
     new_index[(new_type==i)] <- actual_type[i]
   }
   return(new_index)
}


#evaluate accuracy
right_rate <- function(old_type,new_cluster){
 return(sum(old_type==new_cluster)/length(old_type)) 
}


library(rattle)
library(fpc)
data(wine)
set.seed(55)
num_cluster <- 3
newdata <- wine[,-1]
newdata1 <- scale(wine[,-1])
plotcluster(newdata,wine$Type)

label <- k_means_cluster(newdata,3)
plotcluster(newdata,label)
matching_matrix(wine$Type,label,num_cluster)

actual <- c(1,3,2)
new_label <- new_cluster(label,actual)
right_rate(wine$Type,new_label)

##how does scale affects?

label1 <- k_means_cluster(newdata1,3)
plotcluster(newdata1,label1)
#evaluate accuracy
matching_matrix(wine$Type,label1,num_cluster)

actual <- c(1,3,2)
new_label1 <- new_cluster(label1,actual)
right_rate(wine$Type,new_label1)


data(iris)
levels(iris$Species) <- c(1,2,3)
newdata <- iris[,-5]
newdata1 <- scale(iris[,-5])
plotcluster(newdata,wine$Type)

label <- k_means_cluster(newdata,3)
plotcluster(newdata,label)
matching_matrix(iris$Species,label,num_cluster)

actual <- c(1,3,2)
new_label1 <- new_cluster(label,actual)
right_rate(iris$Species,new_label)

##how does scale affects?

label1 <- k_means_cluster(newdata1,3)
plotcluster(newdata1,label1)
#evaluate accuracy
matching_matrix(iris$Species,label1,num_cluster)

actual <- c(1,3,2)
new_label1 <- new_cluster(label1,actual)
right_rate(iris$Species,new_label1)

