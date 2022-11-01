
library(FactoMineR) 
library(Hmisc) 
library(psych) 
library(GGally) 
library(ggplot2) 
library(vioplot) 
library(corrplot) 
library(REdaS) 
library(dbscan) 
library(mclust) 
library(kernlab) 
library(factoextra)
library(ade4) 
library(cluster)

library(fpc) 

setwd("D:/Documents/UIUC/Assignments/Data stats")

################################################################################
crime <- read.csv("CommunitiesDataSetDeletedCols.csv")

dim(crime)

#names(crime)
str(crime)
names(crime)
#taking only numeric variables
crime_num <- crime[,c(5:24,56,100)]

#missing values
sum(is.na(crime_num))
#0

names(crime_num)
#make sure data is scaled, so normalize the data:
library(cluster)
library(factoextra)

crimenum_scaled <- scale(crime_num)

#K-Means Clustering
#how many clusters?:
fviz_nbclust(crimenum_scaled, kmeans)
#no. of clusters must be 4 from the plot

#Running K-Means Cluster Analysis

set.seed(200)
crime_kmean <- kmeans(crimenum_scaled, centers=3, iter.max=200, nstart = 15)
crime_kmean

# Cluster size
crime_kmean$size

# Cluster means
crime_kmean$centers


# Visualize
library("factoextra")
fviz_cluster(crime_kmean, data = crimenum_scaled, 
             ellipse.type = "convex",
             palette = "jco",
             repel = TRUE,
             ggtheme = theme_minimal())

################################################################################

#Another Way to Choose Optimal Clusters

#Enter name of the data matrix to be clustered here:
my.data.matrix <- crimenum_scaled  

my.k.choices <- 1:10
n <- length(my.data.matrix[,1])
wss1 <- (n-1)*sum(apply(my.data.matrix,2,var))
wss <- numeric(0)
for(i in my.k.choices) {
  W <- sum(kmeans(my.data.matrix,i)$withinss)
  wss <- c(wss,W)
}
wss <- c(wss1,wss)
plot(c(1,my.k.choices),wss,type='l',xlab='Number of clusters', 
     ylab='Within-groups sum-of-squares', lwd=3)
# plot shows prominent bend at 3 clusters only, so keeping 3 major clusters