# Delete all the objects in the workspace 
rm(list=ls())
#Problem 3
setwd("C:/Users/Administrator/W_all_pt1/Desktop/hw3")
#Loading the SwissBankNotes provided from UBlearns
load("C:/Users/Administrator/W_all_pt1/Desktop/hw3/SwissBankNotes.rdata")

############combined data counterfeit and geniuine banknotes###############################
#Perform PCA - eigen value decomposition need a positive semi-definite matrix/ singular value decomposition-hold them on the diagonal-derive PC using SVD
SwissBankNotes.pca=prcomp(SwissBankNotes, center = TRUE, scale = TRUE)
names(SwissBankNotes.pca) # need to scale the data  PC1 is large and then it tampers off
summary(SwissBankNotes.pca)# loadings(obervations) are the rotations and the scores are x
#Proportion of variance- big jump 26 and then 7 -retain the first the first 2 compenents
#Plot SwissBankNotes.pca
x11()
plot(SwissBankNotes.pca)
#Create a Biplot #detects outliers and generates simulatneous view the loadings(rotations) and the scores(x)
head(SwissBankNotes.pca$rotation)
head(SwissBankNotes.pca$x)

#Create a column for genuine and couterfiet labelled banknotes
SwissBankNotes$Type_banknotes=c(replicate(100,"Genuine"),replicate(100,"Counterfeit"))
#Biplot(combined)
#install.packages("ggfortify")  #PCA plotting using {ggplot2} and {ggfortify}.
library(ggfortify)
library(ggplot2)
autoplot(SwissBankNotes.pca, data = SwissBankNotes, colour = 'Type_banknotes',
         loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3, loadings.colour = "black", loadings.label.colour = "black",
         frame.type = 'norm')

############Using Genuine banknote data###############################
GenuineSwissBankdata=SwissBankNotes[1:100,-7]
GenuineSwissBankdataPCA=prcomp(GenuineSwissBankdata, center = TRUE, scale = TRUE)
GenuineSwissBankdata$Type_banknotes=c(replicate(100,"Genuine"))
x11()
plot(GenuineSwissBankdataPCA)
autoplot(GenuineSwissBankdataPCA, data = GenuineSwissBankdata, colour = 'Type_banknotes',
         loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3, loadings.colour = "black", loadings.label.colour = "black")

############Using counterfeit  banknote data###############################
counterfeitSwissBankdata=SwissBankNotes[101:200,-7]
counterfeitSwissBankdataPCA=prcomp(counterfeitSwissBankdata, center = TRUE, scale = TRUE)
counterfeitSwissBankdata$Type_banknotes=c(replicate(100,"counterfeit"))
x11()
plot(counterfeitSwissBankdataPCA)
autoplot(counterfeitSwissBankdataPCA, data = counterfeitSwissBankdata, colour = 'Type_banknotes',
         loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3, loadings.colour = "black", loadings.label.colour = "black")
   