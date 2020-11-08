# Delete all the objects in the workspace 
rm(list=ls())
#Problem 11a
#load the data
genes.data <-read.csv("C:/Users/lama_/Desktop/EAS507/HW2/Ch10Ex11.csv", header=FALSE)

#Problem 11b
#Cluster the data using different hierachial clusteirng
hc.complete = hclust(as.dist(1 - cor(genes.data)), method = "complete")
x11()
plot(hc.complete, main="Complete Linkage ", xlab="", sub="",ylab="")

hc.single = hclust(as.dist(1 - cor(genes.data)), method = "single")
x11()
plot(hc.single, main="Single Linkage ", xlab="", sub="",ylab="")

hc.average = hclust(as.dist(1 - cor(genes.data)), method = "average")
x11()
plot(hc.average,  main="Average Linkage ", xlab="", sub="",ylab="")

#Problem 11c 
library(dplyr)
library(ggplot2)
healthypatients <- genes.data[,1:20]
mean.healthy <- apply(healthypatients, 1, mean)
diseasedpatients <- genes.data[,21:40]
mean.diseased <- apply(diseasedpatients, 1, mean)
x11()
ggplot(data.frame(mean.healthy, mean.diseased), aes(x=mean.healthy,y=mean.diseased)) + geom_point() + labs(x="Mean healthy patients", y="Mean diseased patients")

genes.data$diff<-abs(mean.healthy-mean.diseased)
genes.data[order(genes.data$diff,decreasing=TRUE),]

