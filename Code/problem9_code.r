
# Problem 9: Data Clusterring
#install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
#install.packages("cluster")
#install.packages("factoextra")
library("cluster")
library("factoextra")

cluster_data <- read.csv("problem9_data.csv")
head(cluster_data)
cluster_data <- na.omit(cluster_data)
# Normalize the data
means <- apply(cluster_data,2,mean)
sds <- apply(cluster_data,2,sd)

data_norm <- scale(cluster_data,center=means,scale=sds)

head(data_norm)
head(means)
# Determine the distance of each data point
distance <- get_dist(data_norm)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))


# Determine the optimal cluster using the Silhouette Method
# function to compute average silhouette for k clusters
avg_sil <- function( k) {
  km.res <- kmeans(data_norm, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(data_norm))
  mean(ss[, 3])
}


# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl( k.values, avg_sil)
png(file="Average_Silhouette_Value_Plot.png")
plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")
dev.off()

png(file="Optimal_Cluster_Number_plot_Silhouette_Average.png")

# Compute the optimal clusters
fviz_nbclust(data_norm, kmeans, k.max=15, diss=NULL, print.summary = TRUE,
             method = "silhouette")

dev.off()


## Compute the clustering based on the optimum cluster determined above (k=2)
# K-means clustering
# +++++++++++++++++++++
km.res <- kmeans(data_norm, centers=2, nstart = 25)

# Visualize kmeans clustering
# use repel = TRUE to avoid overplotting
png(file="Optimal_Cluster_Plot.png")
fviz_cluster(km.res, data_norm, palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw())
dev.off()

fviz_cluster(km.res, cluster_data, palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw())

clusplot(cluster_data, km.res$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
#install.packages('fpc')
#library(fpc)
#plotcluster(data_norm, km.res$cluster)

pam.res2 <- pam(data_norm, 2,  metric = "euclidean", stand = FALSE)
fviz_silhouette(pam.res2, palette = "jco", ggtheme = theme_classic())

pam.res5 <- pam(data_norm, 5,  metric = "euclidean", stand = FALSE)
fviz_silhouette(pam.res5, palette = "jco", ggtheme = theme_classic())

print(km.res$centers)

km.res1 <- kmeans(cluster_data, centers=2, nstart = 25)

fviz_cluster(km.res1, cluster_data, palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw())

print(km.res1$centers)