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
if(!require(factoextra)){install.packages("factoextra")}
library("factoextra")

if(!require(cluster)){install.packages("cluster")}
library("cluster")

##################################################
### Load Dataset                                ##
##################################################
#Read Dataset
df <- USArrests
#remove any missing value that might be present in the data
 

##################################################
### Step 1_ Scaling/Standardizing               ##
##################################################
 



##################################################
###          Linkage Method                     ##
##################################################
#agnes(data, method) - we can calculate dissimilarity with different methods. 
#perform hierarchical clustering using different updating methods 
clustSingle <- agnes(df1, method = "single")
#complete others.


#plot hierachical tree (dendrogram) 
par(mfrow=c(2,2))
pltree(clustSingle, cex = 0.6, hang = -1, main = "Single Linkage")

#Plot other dendrograms using different linkages

 

##################################################
###         Determining Optimal Clusters        ##
##################################################
#Elbow Method
fviz_nbclust(df1, FUN = hcut, method = "wss")
#The default linkage method used in hcut() is "complete linkage"
#produce plot of clusters vs. gap statistic
pltree(clustComplete, cex = 0.6, hang = -1, main = "Complete Linkage")








