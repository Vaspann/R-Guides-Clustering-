#hierarchical clustering  is a method of cluster analysis which seeks to build a 
#hierarchy of clusters.  Strategies for hierarchical clustering generally fall into two types:
#Agglomerative: This is a "bottom-up" approach: each observation starts in its own cluster, 
#and pairs of clusters are merged as one moves up the hierarchy.
#Divisive: This is a "top-down" approach: all observations start in one cluster,
#and splits are performed recursively as one moves down the hierarchy.

#For this guide we will be working with Animals dataset. This data set contains 
#average brain and body weights for 28 species of land animals.

#Load necessary packages
library(tidyverse)
library(ggrepel)
library(factoextra)
library(cluster)
library(NbClust)
library(dendextend) 
library(MASS)

data()

data(Animals)
str(Animals)

#Use log
plot(Animals)
plot(log(Animals))
logAnimals <- log(Animals)
summary(logAnimals)


#Scale the dataset
logAnimals <- scale(logAnimals)

#Compute distances
distances <-  dist(logAnimals, method = "euclidean")

#Hierarchical clustering with method = ward.D2
set.seed(123)
hc <-  hclust(distances, method = "ward.D2") 

#Plot the dendrogram
plot(hc, cex = 0.7, hang = -1)

#After inspecting the dendrogram, optimal number of clusters is 6
#Create cluster object with k = 6
cluster <- cutree(hc, k = 6)
cluster

#Check the distribution 
table(cluster)

#Confirm k = 6
sil <- silhouette(cluster, distances)
fviz_silhouette(sil)

#Look for the optimal number of clusters
nb.fit<-NbClust(logAnimals, distance = "euclidean", min.nc=2, max.nc=8, 
                method = "ward.D2", index = "all")


#Visualise with colours
par(mfrow=c(1,1))
hc.col <- color_branches(as.dendrogram(hc),k=6)
plot(hc.col)

#Isolate clusters
rect.hclust(hc, k = 6, border = "blue")

#Plot and annotate with rownames
logAnimals <- as.data.frame(logAnimals)
logAnimals$cluster <- as.factor(cluster)

ggplot(logAnimals,aes(x=body,y=brain,col=cluster)) +
  geom_point()+
  geom_label_repel(aes(label = rownames(logAnimals)), size = 3)

