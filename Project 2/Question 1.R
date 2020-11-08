# Delete all the objects in the workspace 
rm(list=ls())

#Problem 9a
data("USArrests")
set.seed(123)
hc.complete <- hclust(dist(USArrests), method = "complete")
x11()
plot(hc.complete, main="Complete Linkage ", xlab="", sub="",ylab="")

#Problem 9b
three.clusters = cutree(hc.complete, 3)
three.clusters
state.name = rownames(USArrests)
cluster.data = data.frame(state.name, three.clusters)
#Cluster1
cluster.data[which(cluster.data$three.clusters == 1),]$state.name
#Cluster2
cluster.data[which(cluster.data$three.clusters == 2),]$state.name
#Cluster 3
cluster.data[which(cluster.data$three.clusters == 3),]$state.name
table(three.clusters,three.clusters)

#Problem 9c
std.data = scale(USArrests)
hc.complete.std = hclust(dist(std.data), method = "complete")
x11()
plot(hc.complete.std, main="Complete Linkage ", xlab="", sub="",ylab="")

#Problem 9d
threecluster.std = cutree(hc.complete.std, 3)
threecluster.std
cluster.data.std = data.frame(state.name, threecluster.std)
#belonging to cluster1
cluster.data.std[which(cluster.data.std$threecluster.std == 1),]$state.name
#belonging to cluster2
cluster.data.std[which(cluster.data.std$threecluster.std == 2),]$state.name
#belonging to cluster 3
cluster.data.std[which(cluster.data.std$threecluster.std == 3),]$state.name
table(three.clusters, threecluster.std)
