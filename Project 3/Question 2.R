# Delete all the objects in the workspace 
rm(list=ls())
#Problem 2
#Part a
#load the data from package ISLR
#install.packages("ISLR")
library(ISLR)
data(USArrests)
head(USArrests)
#scale the USArrest data centre and divide by std i.e. z-score of columnns- everthing on same scale for SOM -
data.scaled <- scale((USArrests))
#Perform Hierachical clustering
set.seed(123)
hc.complete <- hclust(dist(data.scaled), method = "complete")
x11()
plot(hc.complete, main="Complete Linkage ", xlab="", sub="",ylab="")
#Cut the dendogram 4 clusters
four.clusters = cutree(hc.complete,k=4)
four.clusters
state.name = rownames(USArrests)
cluster.data = data.frame(state.name, four.clusters)
#Cluster1
cluster.data[which(cluster.data$four.clusters == 1),]$state.name
#Cluster2
cluster.data[which(cluster.data$four.clusters == 2),]$state.name
#Cluster 3
cluster.data[which(cluster.data$four.clusters == 3),]$state.name
#Cluster 4
cluster.data[which(cluster.data$four.clusters == 4),]$state.name

#Part b
# Fit a SOM to the data and present the results (e.g., U-matrix, phase plots if appropriate, hclust on prototypes

library(kohonen)
set.seed(123)
#Create the grid - can create it in the function grid
som_grid <- somgrid(xdim = 6, ydim = 6, topo = "hexagonal") #Set a grid size 5 by 5/ topology(hexqgonal/rectangular)
#fit the SOM using the grid
USArrests.som <- som(data.scaled, grid = som_grid, rlen = 10000) # number of times run through iterations

names(USArrests.som)
codes <- USArrests.som$codes[[1]] # 6 by 6 have 36 V1 is a prototype/ this is stored in list structure
codes
unit<-USArrests.som$unit.classif
unit # First one is close to protoype V2=2
changes<-USArrests.som$changes
changes # give change of the code book vectors as go through the iterations and tell us it we are converging


#Plot the SOM -defauclt is code- 6 by 6 hexogonal grid- all the variables and all code book vectors for rach protoype; scaled data and only magnitude
x11()
plot(USArrests.som, type="codes", main = "USArrests Data")

x11()
plot(USArrests.som, type = "changes", main = "USArrests Data")

x11()
plot(USArrests.som, type = "count", main = "USArrests Data") # count tells us how many different items/cancer map into the different units
#Get impression of the distrubtion of the data points over where they map over the grid

x11() #circles are the prototypes
plot(USArrests.som, type = "mapping") #Like plot with colors ,but can count the dots- gives idea where points sit wrt to prototypes


#Look at a U matrix plot
coolBlueHotRed <- function(n, alpha = 1){rainbow(n, end=4/6, alpha = alpha)[n:1]}
x11() #add palette -not U-matrix it is dimension of SOM-gives impression of distance to nehgiobred- blue are closest ones- the green less/red are highly disimilar
plot(USArrests.som, type = "dist.neighbours", palette.name = coolBlueHotRed)

#Distane neighbour plot; know class labels=cluster=classes
# component plane plots #visualize orginal data over the SOM we fit
for (i in 1:4){
x11()# there are 4 variables;type is property which is variable;property is codes; how varibales change as look thru map
plot(USArrests.som, type = "property", property=codes[,i], main = colnames(codes)[i]) #column names of code matrix
}

#Cluster the informations
d <- dist(codes) #create a distance matrix- d dissmialirty btw codes using euldeian distance 
#Perform the clustering
set.seed(1234)
hc <- hclust(d)

#Visualize the dendogram- derived from the dissimialrity between the code book vectors
x11()
plot(hc)

#Create an object called som_cluster by cutting the tree
som_cluster <- cutree(hc,k=4) # cut at the height

# plot the SOM with the found clusters
#create 4 clolors for background

my_pal <- c("red", "blue", "yellow","orange")
#assign colors to the prototypes 
my_bhcol <- my_pal[som_cluster]

x11()
plot(USArrests.som, type = "mapping", label=row.names(USArrests),cex= 0.5,col = "black", bgcol = my_bhcol)

#SOM can create boundaries 
add.cluster.boundaries(USArrests.som, som_cluster)

