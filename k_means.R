#Perform k-means clustering on iris data set
#Load the packages
library(stats)
library(tidyverse)
library(ggfortify)

#Load data
data(iris)
plot(iris)


#scale if necessary to have a mean of 0 and sd of 1
#for this guide I will use scaled values
#remove species 
scaled_iris <- scale(iris[,-5])
summary(scaled_iris)


#How to choose k (the number of clusters)
#create 10 models and plot the BSS/TSS ratio

k <- list()
for (i in 1:10) {
  k[[i]] <- kmeans(scaled_iris, i)
}
k

#BSS/TSS ratio is a measure of the goodness of the classifiction k-means
#has found. SS stands for sum of squares. Ideally you want a clustering
#that has the properties of internal cohesion and external separation i.e
#the BSS/TSS ratio should approach 1


# (between_SS / total_SS =)
BSS_TSS <- list()
for (i in 1:10) {
  BSS_TSS[[i]] <- k[[i]]$betweenss/k[[i]]$totss
} 

BSS_TSS


#plot the ratios and look for the elbow shape
#the curve flattens after k = 4 
plot(1:10, BSS_TSS, type = "b",
     ylab = "BSS/TSS", xlab = "Clusters k")


#it appears that k = 2,3,4 seem appropriate
for (i in 2:4) {
  plot(iris, col = k[[i]]$cluster)
}

#implement the clustering model with k = 2 and compare results
model_k_2 <- kmeans(scaled_iris, 2)
autoplot(model_k_2,scaled_iris,frame = TRUE)


# k = 3
model_k_3 <- kmeans(scaled_iris, 3)
autoplot(model_k_3,scaled_iris,frame = TRUE)


# k = 4
model_k_4 <- kmeans(scaled_iris, 4)
autoplot(model_k_4,scaled_iris,frame = TRUE)


#There is some overalapping between two clusters for k = 3 as observations in those groups are
#quite similar 

#Since we already know there are 3 species, therefore  k = 3 
#However if we didn't know k then the best option is to plot all variations and decide based 
#on the visuals

#In addition it is always better to have more data to feed your algorithm as more data points 
#will show distinct clusters if there are any and make overlapping less significant 


