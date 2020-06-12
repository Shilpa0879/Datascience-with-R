#Data load
mydata1<-read.csv("/Volumes/Data/Course Content/DS content/Clustering/University Data Hierarchical clustering/Universities.csv")

################################
##data standardization
mydata <- scale(mydata1[,2:7])

d <- dist(mydata, method = "euclidean") #Computing the distance natrix
fit <- hclust(d, method="average") # Building the algorithm # try with 'centroid'
plot(fit) # display dendogram
clusters <- cutree(fit, k=4) # cut tree into 4 clusters
# draw dendogram with red borders around the 4 clusters 
rect.hclust(fit, k=4, border="red")
#Attach the cluster numbers to Uni
clusters=data.frame('Uni'=mydata1[,1],'Cluster' =clusters)

View(clusters)
