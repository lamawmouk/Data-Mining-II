# Delete all the objects in the workspace 
rm(list=ls())
#set directory
setwd("C:/Users/lama_/Desktop/Data Mining 2/HW1")
#arules
#install.packages("arules")
library('arules')
#Get the boston data
library(MASS)
data("Boston")
head(Boston)
#summary(Boston)
#?transactionInfo
# Visualize the Boston data using histograms
#library(ggplot2)
#for(i in 1:ncol(Boston)){
  #x11()
 # print(ggplot(data = Boston, aes(Boston[,i])) + geom_histogram() + xlab(names(Boston)[i]))
#}
#  Grouping categories of the Boston data variables 
# Numeric vectors are discretized/  convert continous variables to discrete column type/

?Boston
data("Boston")

Boston$crim
Boston[["crim"]] <- ordered(cut((Boston[["crim"]]), c(0,0.2,90), labels = c("Low crim","High crim")))
hist(as.numeric(Boston$crim))

Boston$nox
Boston[["nox"]] <- ordered(cut((Boston[["nox"]]), c(0,0.5,0.8), labels = c("Low nox","High nox")))
hist(as.numeric(Boston$nox))

Boston$black
Boston[["black"]] <- ordered(cut((Boston[["black"]]), c(0,350,400), labels = c("Low black","High black")))
hist(as.numeric(Boston$black))

Boston$dis
Boston[["dis"]] <- ordered(cut((Boston[["dis"]]), c(0,4,13), labels = c("Low dis","High dis")))
hist(as.numeric(Boston$dis)) 


Boston$indus
Boston[["indus"]] <- ordered(cut((Boston[["indus"]]), c(0,10,20), labels = c("Low indus","High indus")))
hist(as.numeric(Boston$indus))

Boston$age
Boston[["age"]] <- ordered(cut((Boston[["age"]]), c(0,70,100), labels = c("Low age","High age")))
hist(as.numeric(Boston$age))

Boston$tax
Boston[["tax"]] <- ordered(cut((Boston[["tax"]]), c(0,400,700), labels = c("Low tax","High tax")))
hist(as.numeric(Boston$tax))

Boston$ptratio
Boston[["ptratio"]] <- ordered(cut((Boston[["ptratio"]]), c(0,18,22), labels = c("Low ptratio","High ptratio")))
hist(as.numeric(Boston$ptratio))

Boston$medv
Boston[["medv"]] <- ordered(cut((Boston[["medv"]]), c(0,20,50), labels = c("Low medv","High medv")))
hist(as.numeric(Boston$medv))


Boston$rm
Boston[["rm"]] <- ordered(cut((Boston[["rm"]]), c(0,6,8), labels = c("Low rm","High rm")))
hist(as.numeric(Boston$rm))

Boston$lstat
Boston[["lstat"]] <- ordered(cut((Boston[["lstat"]]), c(0,13,40), labels = c("Low lstat.","High lstat.")))
hist(as.numeric(Boston$lstat))

Boston$zn
Boston[["zn"]] <- ordered(cut((Boston[["zn"]]), c(-1,10,100), labels = c("Low zn","High zn")))
hist(as.numeric(Boston$zn))

Boston$rad
Boston[["rad"]] <- ordered(cut((Boston[["rad"]]), c(0,10,24), labels = c("Low rad.","High rad")))
hist(as.numeric(Boston$rad))

#Convert to a binary incidence matrix
head(Boston)
Boston.BIM = as(data.frame(lapply(Boston, as.character), stringsAsFactors=T), "transactions")
summary(Boston.BIM)

x11()
itemFrequencyPlot(Boston.BIM, support = 0.05, cex.names = 0.8)

# Apply the apriori algorithm   
rules  <- apriori(Boston.BIM, parameter = list(support = 0.05, confidence = 0.8))
summary(rules)

# Part 2c)
# Take a closer look at the different rules
Crime = subset(rules, subset = rhs %in% "crim=Low crim")
Distance = subset(rules, subset = rhs %in% "dis=Low dis")
inspect(head(sort(Crime, by = "lift")))
inspect(head(sort(Distance, by = "lift")))
lowcrimelowDist = subset(rules, subset = (lhs %in% "crim=Low crim")&(rhs %in% "dis=Low dis")& lift>1.2)
inspect(head(sort(lowcrimelowDist, by = "lift")))
# Part 2d)
# Take a closer look at the different rules
lowpupilratio = subset(rules, subset = rhs %in% "ptratio=Low ptratio")
inspect(head(sort(lowpupilratio, by = "lift")))
#Part2e)
library(MASS)
data("Boston")
model = lm(ptratio~., data= Boston)
summary(model)

