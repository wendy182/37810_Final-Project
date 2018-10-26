# Question 3
library(rattle)
data<-wine
colnames(wine)
X<-data[,-1]
#X2<-scale(wine[-1])

# We set two parameters in the function, k which is the number of cluster we want, and X is the dataset we want to cluster
kmclustering<-function(k,X){
  #initialize k centroids by randomly select k data points from the dataset.
  set.seed(123)
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



table<-matrix(0,3,3)
# create a matrix, columns are the real type of wine, rows are conditional on real type, the proportion being clustered as 1,2,3
for (i in 1:3){
  for (j in 1:3)
    table[i,j]<-(sum(X_with_cluster[which(data$Type==i),]$cluster==j))/nrow(data[data$Type==i,])
}
table


accuracy<-(sum(X_with_cluster[which(data$Type==2),]$cluster==1)+sum(X_with_cluster[which(data$Type==3),]$cluster==2)+sum(X_with_cluster[which(data$Type==1),]$cluster==3))/(nrow(data))
accuracy

# From the table, we can see that the algorithm clustered most data with type-1 to cluster-1, most data with type-2 to cluster-3, and most data with type-3 to cluster-2
# Hence we induce from the table that, 





#Irirs
table <- matrix(0,3,3)
rownames(table)<-levels(iris$Species)
X3<-iris[,-5]
a=1
for (i in levels(iris$Species)){
  for (j in 1:3)
    table[a,j]<-(sum(X_with_cluster[which(iris$Species==i),]$cluster==j))/nrow(iris[iris$Species==i,])
  a = a+1
}
table
accuracy<-(sum(X_with_cluster[which(iris$Species=="setosa"),]$cluster==1)+sum(X_with_cluster[which(iris$Species=="versicolor"),]$cluster==3)+sum(X_with_cluster[which(iris$Species=="virginica"),]$cluster==2))/(nrow(data))
accuracy


















