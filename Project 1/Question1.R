# Delete all the objects in the workspace 
rm(list=ls())
#set directory
setwd("C:/Users/lama_/Desktop/Data Mining 2/HW1")
#################################################Part A
#Create the utility matrix
x = matrix(c(4,5,0,5,1,0,3,2,0,3,4,3,1,2,1,0,2,0,1,3,0,4,5,3),nrow=3, ncol=8, byrow=TRUE)
#Create a binary matrix
x[x>0] = 1;
x
#Compute the jx.2ccard distance and cosine similarity between the users
#install.packages(philentropy)
library(philentropy)
distance(x, method="jaccard");jaccard
distance(x, method="cosine");cosine
#################################################Part B
#Create the utility matrix
x2=matrix(c(4,5,0,5,1,0,3,2,0,3,4,3,1,2,1,0,2,0,1,3,0,4,5,3), nrow=3, ncol=8, byrow=TRUE)
#Create a matrix treat ratings 3,4,5 as 1, and ratings 1, 2, and blank as 0.
x2[x2<3] = 0; # makes 0,1,2 as zero's
x2[x2>0] = 1# makes the other non-zero numbers as 1
x2
distance(x2, method="jaccard")
distance(x2, method="cosine")
#################################################Part C
#Create the utility matrix
x3=matrix(c(4,5,NA,5,1,NA,3,2,NA,3,4,3,1,2,1,NA,2,NA,1,3,NA,4,5,3), nrow=3, ncol=8, byrow=TRUE)
AverageofA=(4+5+5+1+3+2)/6;#10/3 exuclding the NA samples
AverageofB=(3+4+3+1+2+1)/6#7/3
AverageofC=(2+1+3+4+5+3)/6#3
x3=matrix(c((2/3),(5/3),0,(5/3),(-7/3),0,(-1/3),(-4/3),
0,(2/3),(5/3),(2/3),(-4/3),(-1/3),(-4/3),0,
(-1),0,(-2),0,0,1,2,0),nrow=3, ncol=8, byrow=TRUE)
x3
distance(x3, method="cosine")

