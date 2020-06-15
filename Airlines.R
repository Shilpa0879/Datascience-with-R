#Load Data
install.packages('readxl')
library(readxl)

Airlines1<-read_xlsx("E:\\R-folder\\Assignments\\Clustering\\EastWestAirlines.xlsx",sheet = 2)
Airlines<-Airlines1[,-c(1,4,5,6,12)]

summary(Airlines)

#Normailization of data
library(caret)
preproc<-preProcess(Airlines)
AirlinesNorm<-predict(preproc,Airlines)
summary(AirlinesNorm)

#Computing the distance matrix
AirDist<-dist(AirlinesNorm, method = "euclidean")

#Hierarchical Clustering
Aircluster<-hclust(AirDist, method = 'ward.D')
plot(Aircluster)

AirClusterCut<-cutree(Aircluster, k=5)
plot(AirClusterCut)

table(AirClusterCut)
tapply(Airlines$Balance, AirClusterCut,mean)
tapply(Airlines$Qual_miles, AirClusterCut,mean)
tapply(Airlines$Bonus_miles, AirClusterCut,mean)
tapply(Airlines$Bonus_trans, AirClusterCut,mean)
tapply(Airlines$Flight_miles_12mo, AirClusterCut,mean)
tapply(Airlines$Flight_trans_12, AirClusterCut,mean)
tapply(Airlines$Days_since_enroll, AirClusterCut,mean)

##K-Means Clustering
library(factoextra)
fviz_nbclust(Airlines,kmeans,method = "wss")+labs(subtitle = "Elbow method")   

##K-Means Clustering
Airkmeans<-kmeans(Airlines,4)
Airkmeans$cluster
Airkmeans$centers

