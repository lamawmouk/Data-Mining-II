# Delete all the objects in the workspace 
rm(list=ls())
#set directory
setwd("C:/Users/lama_/Desktop/Data Mining 2/HW1")
library(ElemStatLearn)
data("marketing")
names(marketing)
str(marketing)
summary(marketing)
#Find the missing NA columns and determine the column mean
list_na <- colnames(marketing)[ apply(marketing, 2, anyNA) ]
average_missing <- apply(marketing[,colnames(marketing) %in% list_na],2,mean,na.rm =  TRUE)
#Replace the NA values with the mean of the column
library(dplyr)
marketing <- marketing %>%  mutate(Marital= ifelse(is.na( Marital), average_missing[1],  Marital),
         Edu= ifelse(is.na(Edu), average_missing[2], Edu),
         Occupation = ifelse(is.na(Occupation),average_missing[3], Occupation),
         Lived      = ifelse(is.na(Lived), average_missing[4], Lived),
         Household  = ifelse(is.na(Household), average_missing[5], Household),
         Status     = ifelse(is.na(Status  ), average_missing[6], Status),
         Home_Type  = ifelse(is.na(Home_Type), average_missing[7], Home_Type),
         Ethnic     = ifelse(is.na(Ethnic), average_missing[8], Ethnic),  
         Language   = ifelse(is.na(Language), average_missing[9],Language))
#training sample (class 1)
marketing$response = 1
training_sample = marketing
#reference_sample (class 0)
size=8993
Income = sample(seq(1,9), size, replace = T)
Sex = sample(c(1,2), size,replace = T)
Marital = sample(seq(1,5), size, replace = T)
Age = sample(seq(1,7), size, replace = T)
Edu = sample(seq(1,6), size, replace = T)
Occupation = sample(seq(1,9), size, replace = T)
Lived = sample(seq(1,5), size, replace = T)
Dual_Income = sample(seq(1,3), size, replace = T)
Household = sample(seq(1,9), size, replace = T)
Householdu18 = sample(seq(1,9), size, replace = T)
Status = sample(seq(1,3), size, replace = T)
Home_Type = sample(seq(1,5), size, replace = T)
Ethnic = sample(seq(1,8), size, replace = T)
Language = sample(seq(1,3), size, replace = T)
referencesample = data.frame(Income,Sex,Marital,Age,Edu,Occupation,Lived,Dual_Income,Household,Householdu18,Status,Home_Type,Ethnic,Language)
referencesample$response= 0
#Combine the real and the simulated data
combineddata = rbind(referencesample, marketing); combineddata$response = as.factor(as.character(combineddata$response))
#install.packages("rpart.plot")
library(rpart.plot)
library(rpart)				    
library(tree)
library(caret)
#fit the model
set.seed(1234)
model.control=rpart.control(minsplit = 70, xval = 10, cp = 0)
model = rpart(response~., combineddata, control=model.control) # method is classification
model$frame$yval = as.factor(rownames(model$frame))
#Plot tree
x11()
plot(model)
text(model,use.n = TRUE,cex = 0.3)
summary(model)
#install.packages("rattle") 
library (rattle)
asRules(model, compact=FALSE)

