#k-medoids computes cluster centroids using medians instead of means
#In practice, if there are no extreme outliers in the dataset then k-means and k-medoids 
#will produce similar results.

#Load the packages
library(kohonen)
library(factoextra)
library(tidyverse)
library(cluster)

#For this example we will use the wine dataset built under the kohonen package 
#The wine dataset contains the results of a chemical analysis of wines grown in a specific area of Italy. 
#A dataset containing 177 rows and thirteen columns; object vintages contains the class labels.

#Load the data
data("wines")
wines <- as.data.frame(wines)
set.seed(123)

#Make sure varibales are continuous
str(wines)
summary(wines)

#plot for visuals and relationships
plot(wines)


#scale if necessary to have a mean of 0 and sd of 1
scaled_wine <- scale(wines)
summary(scaled_wine)
head(scaled_wine)


#to perform k-medoids clustering in R we can use the pam() function,
#which stands for “partitioning around medians”


#how to choose k (the number of clusters)

#1. Number of Clusters vs. the Total Within Sum of Squares

#first, we’ll use the fviz_nbclust() function to create a plot of the 
#number of clusters vs. the total within sum of squares:


fviz_nbclust(scaled_wine, pam, method = "wss", linecolor = "red")

#the total within sum of squares will typically always decrease
#as we increase the number of clusters, so when we create this 
#type of plot we look for an “elbow” where the sum of squares begins 
#to flatten.

#for this plot the elbow occurs at k = 3
#choosing k beyond that will likely result in overfitting


#2. Number of Clusters vs. Gap Statistic

#another way to determine the optimal number of clusters is to use a metric known 
#as the gap statistic, which compares the total intra-cluster variation for different 
#values of k with their expected values for a distribution with no clustering.


#we can calculate the gap statistic for each number of clusters using clusGap function

Gap_stat <- clusGap(scaled_wine, FUN = pam, K.max = 10, B = 50) 


#plot of clusters vs. gap statistic using  fviz_gap_stat function

fviz_gap_stat(Gap_stat)

#again this plot confirms that k = 3 is the optimal number of clusters
#with the highest gap statistics


#now we can perform k-medioids clustering with optimal k
set.seed(123)
model_k_medians <- pam(scaled_wine, k = 3)
model_k_medians


#note that the 3 cluster centroids are actual observations in the dataset
#as we are using medians 

#we can visualize the clusters on a scatterplot that displays the first two principal
#components on the axes using the fivz_cluster

#plot results of k-medoids model
fviz_cluster(model_k_medians, data = scaled_wine)


#since we already know there are 3 vintages, therefore  k = 3 
#However if we didn't know k then the best options would be to plot:
##1. Number of Clusters vs. the Total Within Sum of Squares
##2. Number of Clusters vs. Gap Statistic
#and then decide for k value

#compare our results
table(model_k_medians$clustering, vintages)

#there is signinficant overlap between Barolo and Grignolino vintages due to similar measurements

#in addition it is always better to have more data to feed your algorithm as more data points 
#will show distinct clusters if there are any and make overlapping less significant 

