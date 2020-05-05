#Load data
mydata1<-read.csv("E:\\R-folder\\CSV files\\Universities.csv")

mydata<-scale(mydata1[,2:7])

#Computing the distance matrix
d<-dist(mydata,method = "euclidean") 
d

#Building the algorithm, try with centroid
#hclust means hierarchical cluster and avg is type
fit<-hclust(d,method = "average")  
plot(fit)  #display dendogram

clusters<-cutree(fit,k=4) #cut tree into 4 clusters

#draw dendogram with red borders around the 4 clusters
rect.hclust(fit,k=4,border="red")

#Attach the clusters numbers to Uni 
cluster=data.frame('Uni'=mydata1[,1],'Cluster'=clusters)
            
#Elbow method
install.packages('factoextra')
library(factoextra)
fviz_nbclust(mydata1[-1],kmeans,method = "wss")+labs(subtitle = "Elbow method")   

##Cluster algorithm building
km<-kmeans(mydata1[,-1],4)  ##4 indicates  4 nos rows
km$centers
km$cluster
clust<-data.frame("university"=mydata1[,1],"cluster"=km$cluster)

##Animation
install.packages("animation")
library(animation)
windows()
km<-kmeans.ani(mydata1[,-1],3)  ##3 indicates - 3 nos data cluster


        