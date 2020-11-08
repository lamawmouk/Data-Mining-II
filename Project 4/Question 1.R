# Delete all the objects in the workspace 
rm(list=ls())
#Problem 1
library(igraphdata)
library(igraph)
data(karate)
#karate
data(kite)
#?kite	
#########################Karate############################################
#Number of edges in the karate
edges_of_karate=gsize(karate)
edges_of_karate
# 5% deletion of edges
deleted_edges=sample(E(karate),round((0.05)*edges_of_karate))
deleted_edges
remaining_edges= delete.edges(karate, deleted_edges)
#Creating an array for the deleted edges
V1= get.vertex.attribute(karate)$name
V2=1:length(V1)
df= data.frame(V1,V2);df
all.edges=get.edgelist(karate)
all_edges_array=as.data.frame(all.edges)
deleted_edges_array=as.data.frame(all_edges_array[deleted_edges,])
#Merge the original table with the deleted edges table using first column
df2= merge(deleted_edges_array,df,by='V1')
#remove column 1 after merging with the column 1 numbers
df2=subset(df2,select=-c(1))
#Rename the column names to merge with original data df which has V1 and V2
colnames(df2)=c('V1','V2')
#Merge the original table with the deleted edges table using first column
df3= merge(df2,df,by='V1')
#Remove column 1 as after merging we get the numbers of column 1
df3=subset(df3,select=-c(1))
colnames(df3)=c('V1','V2')
#Predict the pobability of edges being missing
set.seed(123)
pred_karate =predict_edges(remaining_edges)
pred_karate$edges
pred_karate$prob
x11()
plot(pred_karate$prob)
# Table showing the deleted edges,the predicted probabilities and the rank
rank=1:dim(pred_karate$edges)
combined= cbind(rank,pred_karate$edges,pred_karate$prob)
colnames(combined)=c('V3','V1','V2','V4')
tableedges=merge(df3,combined)
colnames(tableedges)=c('Node1','Node2','Rank', 'Probofedgemissing')
tableedges;deleted_edges
#########################Karate############################################
#Number of edges in the karate
edges_of_karate=gsize(karate)
edges_of_karate
# 15% deletion of edges
deleted_edges=sample(E(karate),round((0.15)*edges_of_karate))
deleted_edges
remaining_edges= delete.edges(karate, deleted_edges)
#Creating an array for the deleted edges
V1= get.vertex.attribute(karate)$name
V2=1:length(V1)
df= data.frame(V1,V2);df
all.edges=get.edgelist(karate)
all_edges_array=as.data.frame(all.edges)
deleted_edges_array=as.data.frame(all_edges_array[deleted_edges,])
#Merge the original table with the deleted edges table using first column
df2= merge(deleted_edges_array,df,by='V1')
#remove column 1 after merging with the column 1 numbers
df2=subset(df2,select=-c(1))
#Rename the column names to merge with original data df which has V1 and V2
colnames(df2)=c('V1','V2')
#Merge the original table with the deleted edges table using first column
df3= merge(df2,df,by='V1')
#Remove column 1 as after merging we get the numbers of column 1
df3=subset(df3,select=-c(1))
colnames(df3)=c('V1','V2')
#Predict the pobability of edges being missing
set.seed(123)
pred_karate =predict_edges(remaining_edges)
pred_karate$edges
pred_karate$prob
x11()
plot(pred_karate$prob)
# Table showing the deleted edges,the predicted probabilities and the rank
rank=1:dim(pred_karate$edges)
combined= cbind(rank,pred_karate$edges,pred_karate$prob)
colnames(combined)=c('V3','V1','V2','V4')
tableedges=merge(df3,combined)
colnames(tableedges)=c('Node1','Node2','Rank','Probofedgemissing')
tableedges;deleted_edges
#########################Karate############################################
#Number of edges in the karate
edges_of_karate=gsize(karate)
edges_of_karate
# 40% deletion of edges
deleted_edges=sample(E(karate),round((0.40)*edges_of_karate))
deleted_edges
remaining_edges= delete.edges(karate, deleted_edges)
#Creating an array for the deleted edges
V1= get.vertex.attribute(karate)$name
V2=1:length(V1)
df= data.frame(V1,V2);df
all.edges=get.edgelist(karate)
all_edges_array=as.data.frame(all.edges)
deleted_edges_array=as.data.frame(all_edges_array[deleted_edges,])
#Merge the original table with the deleted edges table using first column
df2= merge(deleted_edges_array,df,by='V1')
#remove column 1 after merging with the column 1 numbers
df2=subset(df2,select=-c(1))
#Rename the column names to merge with original data df which has V1 and V2
colnames(df2)=c('V1','V2')
#Merge the original table with the deleted edges table using first column
df3= merge(df2,df,by='V1')
#Remove column 1 as after merging we get the numbers of column 1
df3=subset(df3,select=-c(1))
colnames(df3)=c('V1','V2')
#Predict the pobability of edges being missing
set.seed(123)
pred_karate =predict_edges(remaining_edges)
pred_karate$edges
pred_karate$prob
x11()
plot(pred_karate$prob)
# Table showing the deleted edges,the predicted probabilities and the rank
rank=1:dim(pred_karate$edges)
combined= cbind(rank,pred_karate$edges,pred_karate$prob)
colnames(combined)=c('V3','V1','V2','V4')
tableedges=merge(df3,combined)
colnames(tableedges)=c('Node1','Node2','Rank', 'Probofedgemissing')
tableedges;deleted_edges

#########################################################################
#########################kite############################################
#Number of edges in the kite
edges_of_kite=gsize(kite)
edges_of_kite
# 5% deletion of edges
deleted_edges=sample(E(kite),round((0.05)*edges_of_kite))
deleted_edges
remaining_edges= delete.edges(kite, deleted_edges)
#Creating an array for the deleted edges
V1= get.vertex.attribute(kite)$name
V2=1:length(V1)
df= data.frame(V1,V2);df
all.edges=get.edgelist(kite)
all_edges_array=as.data.frame(all.edges)
deleted_edges_array=as.data.frame(all_edges_array[deleted_edges,])
#Merge the original table with the deleted edges table using first column
df2= merge(deleted_edges_array,df,by='V1')
#remove column 1 after merging with the column 1 numbers
df2=subset(df2,select=-c(1))
#Rename the column names to merge with original data df which has V1 and V2
colnames(df2)=c('V1','V2')
#Merge the original table with the deleted edges table using first column
df3= merge(df2,df,by='V1')
#Remove column 1 as after merging we get the numbers of column 1
df3=subset(df3,select=-c(1))
colnames(df3)=c('V1','V2')
#Predict the pobability of edges being missing
set.seed(123)
pred_kite =predict_edges(remaining_edges)
pred_kite$edges
pred_kite$prob
x11()
plot(pred_kite$prob)
# Table showing the deleted edges,the predicted probabilities and the rank
rank=1:dim(pred_kite$edges)
combined= cbind(rank,pred_kite$edges,pred_kite$prob)
colnames(combined)=c('V3','V1','V2','V4')
tableedges=merge(df3,combined)
colnames(tableedges)=c('Node1','Node2','Rank','Probofedgemissing')
tableedges;deleted_edges
#########################kite############################################
#Number of edges in the kite
edges_of_kite=gsize(kite)
edges_of_kite
# 15% deletion of edges
deleted_edges=sample(E(kite),round((0.15)*edges_of_kite))
deleted_edges
remaining_edges= delete.edges(kite, deleted_edges)
#Creating an array for the deleted edges
V1= get.vertex.attribute(kite)$name
V2=1:length(V1)
df= data.frame(V1,V2);df
all.edges=get.edgelist(kite)
all_edges_array=as.data.frame(all.edges)
deleted_edges_array=as.data.frame(all_edges_array[deleted_edges,])
#Merge the original table with the deleted edges table using first column
df2= merge(deleted_edges_array,df,by='V1')
#remove column 1 after merging with the column 1 numbers
df2=subset(df2,select=-c(1))
#Rename the column names to merge with original data df which has V1 and V2
colnames(df2)=c('V1','V2')
#Merge the original table with the deleted edges table using first column
df3= merge(df2,df,by='V1')
#Remove column 1 as after merging we get the numbers of column 1
df3=subset(df3,select=-c(1))
colnames(df3)=c('V1','V2')
#Predict the pobability of edges being missing
set.seed(123)
pred_kite =predict_edges(remaining_edges)
pred_kite$edges
pred_kite$prob
x11()
plot(pred_kite$prob)
# Table showing the deleted edges,the predicted probabilities and the rank
rank=1:dim(pred_kite$edges)
combined= cbind(rank,pred_kite$edges,pred_kite$prob)
colnames(combined)=c('V3','V1','V2','V4')
tableedges=merge(df3,combined)
colnames(tableedges)=c('Node1','Node2','Rank','Probofedgemissing')
tableedges;deleted_edges
#########################kite############################################
#Number of edges in the kite
edges_of_kite=gsize(kite)
edges_of_kite
# 15% deletion of edges
deleted_edges=sample(E(kite),round((0.40)*edges_of_kite))
deleted_edges
remaining_edges= delete.edges(kite, deleted_edges)
#Creating an array for the deleted edges
V1= get.vertex.attribute(kite)$name
V2=1:length(V1)
df= data.frame(V1,V2);df
all.edges=get.edgelist(kite)
all_edges_array=as.data.frame(all.edges)
deleted_edges_array=as.data.frame(all_edges_array[deleted_edges,])
#Merge the original table with the deleted edges table using first column
df2= merge(deleted_edges_array,df,by='V1')
#remove column 1 after merging with the column 1 numbers
df2=subset(df2,select=-c(1))
#Rename the column names to merge with original data df which has V1 and V2
colnames(df2)=c('V1','V2')
#Merge the original table with the deleted edges table using first column
df3= merge(df2,df,by='V1')
#Remove column 1 as after merging we get the numbers of column 1
df3=subset(df3,select=-c(1))
colnames(df3)=c('V1','V2')
#Predict the pobability of edges being missing
set.seed(123)
pred_kite =predict_edges(remaining_edges)
pred_kite$edges
pred_kite$prob
x11()
plot(pred_kite$prob)
# Table showing the deleted edges,the predicted probabilities and the rank
rank=1:dim(pred_kite$edges)
combined= cbind(rank,pred_kite$edges,pred_kite$prob)
colnames(combined)=c('V3','V1','V2','V4')
tableedges=merge(df3,combined)
colnames(tableedges)=c('Node1','Node2','Rank','Probofedgemissing')
tableedges;deleted_edges


