# Delete all the objects in the workspace 
rm(list=ls())
#Problem 1
setwd("C:/Users/Administrator/W_all_pt1/Desktop/hw3")
#load package which has nci data
#install.packages("ElemStatLearn") #package 'ElemStatLearn' is not available (for R version 3.6.3)
#library(ElemStatLearn)
load(("C:/Users/Administrator/W_all_pt1/Desktop/hw3/nci.RData"))
head(nci)
unique(colnames(nci))
#scale the nci data centre and divide by std i.e. z-score of columnns- everthing on same scale for SOM -
nci.scaled <- scale(t(nci))

#Fit an SOM
#install.packages("kohonen")
library(kohonen)
set.seed(123)
#Create the grid - can create it in the function grid
som_grid <- somgrid(xdim = 5, ydim = 5, topo = "hexagonal") #Set a grid size 5 by 5/ topology(hexqgonal/rectangular)
#fit the SOM using the grid
nci.som <- som(nci.scaled, grid = som_grid, rlen = 3000) # number of times run through iterations

names(nci.som)
codes <- nci.som$codes[[1]] # 5 by 5 have 25 V1 is a prototype/ this is stored in list structure
codes
unit<-nci.som$unit.classif
unit # First one is close to protoype V2=2
changes<-nci.som$changes
changes # give change of the code book vectors as go through the iterations and tell us it we are converging

?plot.kohonen

#Plot the SOM -defauclt is code- 5 by 5 hexogonal grid- all the variables and all code book vectors for rach protoype; scaled data and only magnitude
x11()
plot(nci.som, type="codes", main = "Nci Data")

x11()
plot(nci.som, type = "changes", main = "Nci Data")

x11()
plot(nci.som, type = "count", main = "Nci Data") # count tells us how many different items/cancer map into the different units
#Get impression of the distrubtion of the data points over where they map over the grid

x11() #circles are the prototypes
plot(nci.som, type = "mapping",label=row.names(nci.scaled),cex=0.5) #Like plot with colors ,but can count the dots- gives idea where points sit wrt to prototypes


#Look at a U matrix plot
coolBlueHotRed <- function(n, alpha = 1){rainbow(n, end=4/6, alpha = alpha)[n:1]}
x11() #add palette -not U-matrix it is dimension of SOM-gives impression of distance to nehgiobred- blue are closest ones- the green less/red are highly disimilar
plot(nci.som, type = "dist.neighbours", palette.name = coolBlueHotRed)

#Distane neighbour plot; know class labels=cluster=classes
# component plane plots #visualize orginal data over the SOM we fit
#for (i in 1:64){
  #x11()# there are 64 varibales;type is property which is variable;property is codes; how varibales change as look thru map
  #plot(nci.som, type = "property", property=codes[,i], main = colnames(codes)[i]) #column names of code matrix
#}


#Cluster the informations
d <- dist(codes) #create a distance matrix- d dissmialirty btw codes using euldeian distance 
#Perform the clustering
hc <- hclust(d)

#Visualize the dendogram- derived from the dissimialrity between the code book vectors
x11()
plot(hc)

#Create an object called som_cluster by cutting the tree
som_cluster <- cutree(hc, h=130) # cut at the height

# plot the SOM with the found clusters
#there are three groups here -know that from dendogram
#create 3 clolors for background

my_pal <- c("red", "blue", "yellow")
#assign colors to the prototypes 
my_bhcol <- my_pal[som_cluster]



x11()
plot(nci.som, type = "mapping", label=row.names(nci.scaled),cex= 0.5,col = "black", bgcol = my_bhcol)
#SOM can create boundaries 
add.cluster.boundaries(nci.som, som_cluster)

