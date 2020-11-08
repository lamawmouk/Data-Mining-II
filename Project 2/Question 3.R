# Delete all the objects in the workspace 
rm(list=ls())
#Problem 3

#install.packages("fpc")
#install.packages("bootcluster")

library("fpc")
library("bootcluster")
library("fossil")
#load the seed data
seeddata<- read.delim("C:/Users/lama_/Desktop/EAS507/HW2/Seeddata.txt.txt")
newdata <- seeddata[,-8]
#cluster the data based on single-linkage, average linkage, and complete-linkage agglomerative hierarchical clustering
d <- dist(newdata)
dim(as.matrix(d))
hc.complete=hclust(dist(newdata), method ="complete")
hc.average=hclust(dist(newdata), method ="average")
hc.single=hclust(dist(newdata), method ="single")
#plot the cluster based on the observations
x11()
plot(hc.complete,main="Complete Linkage ", xlab="",sub="", cex=.7,labels = seeddata$Seed.Group)
x11()
plot(hc.average , main="Average Linkage", xlab="", sub="",  cex=.7,labels = seeddata$Seed.Group)
x11()
plot(hc.single , main="Single Linkage ",sub="",  cex=.7,labels = seeddata$Seed.Group)
# Determine the value for k using adjusted rand index
library("fossil")
storecomplete.adjrad <- c()
for (i in 2:6){
  ct <- cutree(hc.complete, k=i)
  adjrandindex <-adj.rand.index(ct,as.numeric(seeddata$Seed.Group))
  storecomplete.adjrad <- c(storecomplete.adjrad, adjrandindex )
}
storecomplete.adjrad
#0.6530775 0.5200210 0.4623355 0.5020200 0.5007037

storeaverage.adjrad <- c()
for (i in 2:6){
  ct <- cutree(hc.average, k=i)
  adjrandindex <-adj.rand.index(ct,as.numeric(seeddata$Seed.Group))
  storeaverage.adjrad <- c(storeaverage.adjrad, adjrandindex )
}
storeaverage.adjrad
#0.8703981 0.7482942 0.6562729 0.6407289 0.6177764

storesingle.adjrad <- c()
for (i in 2:6){
  ct <- cutree(hc.single, k=i)
  adjrandindex <-adj.rand.index(ct,as.numeric(seeddata$Seed.Group))
  storesingle.adjrad <- c(storesingle.adjrad, adjrandindex )
}
storesingle.adjrad
#0.519117824 0.001509422 0.001208842 0.001487219 0.001396953

##############################
## k-mediods
##############################
kmed <- pamk(newdata,2)
# let the program decide optimal k
kmed$nc ;kmed
# tabulate the results as before
table(kmed$pamobject$clustering, seeddata$Seed.Group)
# plot the results for k= 2
x11()
layout(matrix(c(1,2), 1, 2))
plot(kmed$pamobject)

# try k= 3
kmed3 <- pamk(newdata, 3)
table(kmed3$pamobject$clustering,seeddata$Seed.Group)
# plot the results for k= 2
x11()
layout(matrix(c(1,2), 1, 2))
plot(kmed3$pamobject)

# calculate rand index and adjusted rand index for k-medoids
store.adjrandind.kmed <- c()
for (i in 2:6){
  kmed <- pamk(newdata, i)
  adjrandind <-adj.rand.index(kmed$pamobject$clustering, as.numeric(seeddata$Seed.Group))
  store.adjrandind.kmed <- c(store.adjrandind.kmed,adjrandind)
}
store.adjrandind.kmed


