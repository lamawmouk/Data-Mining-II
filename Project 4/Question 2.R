# Delete all the objects in the workspace 
rm(list=ls())
#Problem 2
#Installing the packages required for gRBase
#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install("graph")

#if (!requireNamespace("BiocManager", quietly = TRUE))
#install.packages("BiocManager")
#BiocManager::install("Rgraphviz")

#if (!requireNamespace("BiocManager", quietly = TRUE))
# install.packages("BiocManager")
#BiocManager::install("RBGL")

#install.packages("gRbase")

#load the libraries
library(Rgraphviz)
library(gRbase)
library(gRain)
library(ggm)


CPD=list(~A,~B,~C|A,~D|A:B,~E|B,~F|C:A:E,~G|D:E,~H|F:G)
CPD_graph=dagList(CPD)
x11()
plot(CPD_graph)
dSep(as(CPD_graph,"matrix"),"C","G",cond = NULL)
dSep(as(CPD_graph,"matrix"),"C","E",cond = NULL)
dSep(as(CPD_graph,"matrix"),"C","E",c("G"))
dSep(as(CPD_graph,"matrix"),"A","G",c("D","E"))
dSep(as(CPD_graph,"matrix"),"A","G",c("D"))

