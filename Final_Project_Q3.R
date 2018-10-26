

# Question 3

library(rattle)
data<-wine
colnames(wine)
X<-data[,-1]
colnames(X)

# We set two parameters in the function, k which is the number of cluster we want, and X is the dataset we want to cluster
kmclustering<-function(k,X){
  #initialize k centroids by randomly select k data points from the dataset.
  set.seed(13)
  initial_ind = sample(1:nrow(data),k,replace=F)
  initial_cen = X[initial_ind,]
  initial_con = TRUE
  
  #create a centroid table used to store and update centroids
  centroid_table=data.frame(initial_cen,cluster=1:k)
  #reindex centroid table
  rownames(centroid_table)<-1:nrow(centroid_table)
  
  #create a new dataframe with the column 'cluster'
  X_with_cluster<-data.frame(X,cluster=0)
  
  while(initial_con){
    X_cluster_last <-X_with_cluster$cluster
   #calculate euclidean distance between observations and each centroid, assign cluster according to minimum distance
    for (i in 1:nrow(X)){
      dist<-0
      for (j in 1:ncol(X)){
       dist<-dist+(X[i,j]-centroid_table[,j])^2
       }
      X_with_cluster[i,]$cluster<-which.min(sqrt(dist))
    }
    X_cluster_updated <- X_with_cluster$cluster
   #update the centroid in each cluster
    for (i in 1:k){
      centroid_table[i,]<-colMeans(X_with_cluster[X_with_cluster$cluster==i,])}
    
    if (all(X_cluster_updated==X_cluster_last)) initial_con=FALSE}
  
   #when no datapoints change clusters anymore, set condition to TRUE and exit loop
  return (X_with_cluster)
}


X_with_cluster<-kmclustering(3,X)

#install.packages('fpc')
library('fpc')

plotcluster(X, X_with_cluster$cluster)

plotcluster(X,data$Type)



#define successful rate = mean(correctly cate as type I/all type I+correctly cate as type II/all type II+correctly cate as type III/all typeIII)

which(X_with_cluster$cluster==1) %in% which(data$Type==1)






