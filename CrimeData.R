# Load Data

Crime_Data1<-read.csv("E:\\R-folder\\Assignments\\Clustering\\crime_data.csv")
Crime_Data<-scale(Crime_Data1[,2:5])

#Computing the distance matrix
d<-dist(Crime_Data, method = "euclidean")
d

#hclust means hierarchical cluster and avg is type
fit<-hclust(d, method = 'average')
plot(fit)

clusters<-cutree(fit, k=4)

#draw dendogram with red borders around the 4 clusters
rect.hclust(fit, k=4, border = "red")

#Attach the clusters numbers to Uni 
Cluster=data.frame('Location'=Crime_Data1[,1], Cluster=clusters)


