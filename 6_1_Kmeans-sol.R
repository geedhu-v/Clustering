##################################################
### PROG8430                                    ##
### Data Reduction Demonstration                ##
### Demonstration - SFS                         ##  
##################################################
#                                               ##
##################################################
# Written by Peiyuan Zhou
# ID: 123456789
##################################################
### Basic Set Up                                ##
##################################################
# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014") 
# Clean workspace
rm(list=ls())

##################################################
### Install Libraries                           ##
##################################################
if(!require(caret)){install.packages("caret")}
library("caret")

if(!require(factoextra)){install.packages("factoextra")}
library("factoextra")

##################################################
### Load Dataset                                ##
##################################################
#Read Dataset
df <- USArrests
#remove any missing value that might be present in the data
df <- na.omit(df)
head(df)

par(mfrow=c(2,2))
for (i in 1:ncol(df)) {
  if (is.numeric(df[,i])) {
      boxplot(df[i], main=names(df)[i],
              horizontal=TRUE, pch=10)
  }
}

##################################################
### Step 1_ Scaling/Standardizing               ##
##################################################
#Method 1: Writing functions
#Create a quick normalization function
norm1 <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#A different standardization function
norm2 <- function(x) {
  return ((x-mean(x))/sd(x))
}
USArrests$Murder_MinMax <- norm1(USArrests$Murder)
USArrests$Murder_Normx <- norm2(USArrests$Murder)
USArrests$Murder_MinMax
USArrests$Murder_Normx


#Method 2: Using built-in functions.
process <- preProcess(df,method=c("range"))
df1<-predict(process,as.data.frame(df)) # need library(caret)
df1 # by using preProcess() function, all variables are normalized same as norm1.

df2 <- scale(df)
head(df2) # by using scale() function, all variables are standardized same as norm2.

##################################################
### Clustering Distance Measures                ##
##################################################
distance <- dist(df1)

#another function for computing distance.
distance <- get_dist(df1) # need the package(factoextra) 
distance
#get_dist: for computing a distance matrix between the rows of a data matrix. 
#The default distance computed is the Euclidean; 
#get_dist also supports distanced described in equations 2-5 above plus others.

#Visualize - method 1 - use library(factoextra)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
#fviz_dist: for visualizing a distance matrix

#################################################
#Visualize - method 2 - built-in function image()
par(mfrow=c(1,1))
distance <- data.matrix(distance)
dim <- ncol(distance)
image(1:dim, 1:dim, distance, axes = FALSE,xlab="",ylab="")
axis(1, 1:dim, colnames(distance), cex.axis = 0.5, las=3)
axis(2, 1:dim, colnames(distance), cex.axis = 0.5, las=1)

##################################################
### Step 2: K-means Clustering                  ##
##################################################
# Computing K-means clustering in R
k <- kmeans (df1, centers = 2, nstart = 25,iter.max = 10)
# centers=2 means k=2 - two clusters
# "nstart" attempts multiple initial configurations and reports on the best one
# nstart = 25 which means running 25 times with different initializations, and choose the best one.

#Check outputs and understand the meaning of outputs. 
k
str(k)

#Visualize the results ----- Method 1----
plot(df1$Murder, df1$Assault,
     col=k$cluster, pch=as.numeric(k$cluster))
centers <- data.frame(cluster=factor(1:2), k$centers) #k=2
points(centers$Murder, centers$Assault,
       col=centers$cluster, pch=as.numeric(centers$cluster),
       cex=3, lwd=3)


#Visualize the results ----- Method 2----fviz_cluster() in package(caret)
fviz_cluster(k, data = df1)
# fviz_cluster will perform principal component analysis (PCA)  
# plot the data points according to the first two principal components 


##################################################
### Step 3: Clustering with different k         ##
##################################################
k3 <- kmeans(df1, centers = 3, nstart = 25)
k4 <- kmeans(df1, centers = 4, nstart = 25)
k5 <- kmeans(df1, centers = 5, nstart = 25)

# Visulization - Use fviz_cluster() function
p1 <- fviz_cluster(k, geom = "point", data = df1,ellipse = TRUE) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df1,ellipse = TRUE) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df1,ellipse = TRUE) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df1,ellipse = TRUE) + ggtitle("k = 5")
library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

k
k3
k4
k5

##################################################
### Step 4: Determining Optimal Clusters        ##
##################################################
###Elbow in WSS result
#function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(df1, k, nstart = 25)$tot.withinss
}
k.values <-1:15
# extract wss for 2-15 clusters
wss_values <- sapply(k.values, wss)

par(mfrow = c(1, 1))
plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     main="Elbow Chart for Clusters",
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# another function that can be used for plotting "wss".
fviz_nbclust(df1, FUN = kmeans, method = "wss")

##################################################
### Apply the optimal number of clusters        ##
##################################################
#k=4 is the best.
#Visualize the results for k=4
plot(df1$Murder, df1$Assault,
     col=k4$cluster, pch=as.numeric(k4$cluster))
centers <- data.frame(cluster=factor(1:4), k4$centers) #k=2
points(centers$Murder, centers$Assault,
       col=centers$cluster, pch=as.numeric(centers$cluster),
       cex=3, lwd=3)

#Another visulization
fviz_cluster(k4, data = df1)


##*Output the mean for each cluster in original data. 
data_with_clusters <- cbind(df, Cluster = k4$cluster)
cluster_means <- aggregate(. ~ Cluster, data = data_with_clusters, mean)
















