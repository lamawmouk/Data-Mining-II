# Delete all the objects in the workspace 
rm(list=ls())
#Problem 3
library(Rgraphviz)
library(gRbase)
library(gRain)
library(ggm)
library(gRim)
library(graph)
#Load the data
data(cad1)
names(cad1)
####################Parta##################################
#Plot the Baeysian Network in R using the CPDs
cad = list(~Sex, ~Smoker|Sex, ~Inherit|Smoker,~SuffHeartF, ~Hyperchol|SuffHeartF:Smoker, ~CAD|Inherit:Hyperchol)
cad_dag = dagList(cad)
x11()
plot(cad_dag)
#Conditional Probability Tables 
cpd_tables=extractCPT(cad1, cad_dag)
#D-separation
dSep(as(cad_dag, "matrix"),"Sex", "SuffHeartF",cond = NULL)
dSep(as(cad_dag, "matrix"), "SuffHeartF","Smoker",cond = NULL)
dSep(as(cad_dag, "matrix"), "SuffHeartF","Inherit",cond = NULL)
dSep(as(cad_dag, "matrix"), "Sex", "CAD", c("Smoker"))
dSep(as(cad_dag, "matrix"), "Sex", "Hyperchol", c("Smoker"))
dSep(as(cad_dag, "matrix"), "Sex", "SuffHeartF", c("Smoker"))
dSep(as(cad_dag, "matrix"),"Inherit", "Hyperchol",c("Smoker"))
dSep(as(cad_dag, "matrix"),"Inherit", "Sex",c("Smoker"))
dSep(as(cad_dag, "matrix"),"Inherit", "SuffHeartF",c("Smoker"))
dSep(as(cad_dag, "matrix"), "Hyperchol","Sex", c("Smoker", "SuffHeartF"))
dSep(as(cad_dag, "matrix"), "Hyperchol","Inherit", c("Smoker", "SuffHeartF"))
dSep(as(cad_dag, "matrix"), "SuffHeartF", "CAD", c("Hyperchol","Smoker"))
dSep(as(cad_dag, "matrix"),"SuffHeartF", "Inherit",c("Hyperchol","Smoker"))
dSep(as(cad_dag, "matrix"),"Smoker", "CAD",c("Inherit", "Hyperchol"))
dSep(as(cad_dag, "matrix"),"SuffHeartF", "CAD",c("Inherit", "Hyperchol"))

####################Partb##################################
plist <- compileCPT(list(cpd_tables$Sex,cpd_tables$Smoker,cpd_tables$SuffHeartF,cpd_tables$Inherit,cpd_tables$Hyperchol,cpd_tables$CAD ))
grn1 <- grain(plist)
#summary(grn1)

### Compile the network
grn1_compile = compile(grn1)
#summary(grn1_compile)

### Propagate the the network
grn1_compile = propagate(grn1_compile)
#summary(grn1_compile)

### After absorbing the evidence
grn1_compile_after = setFinding(grn1_compile, nodes = c("Sex", "Hyperchol"), states = c("Female", "yes"))
#getFinding(grn1_compile_after)
#Joint 
querygrain(grn1_compile, nodes = c("SuffHeartF", "CAD"), type = "joint")
querygrain(grn1_compile_after, nodes = c("SuffHeartF", "CAD"), type = "joint")
#Marginal 
querygrain(grn1_compile, nodes = c("SuffHeartF", "CAD"), type = "marginal")
querygrain(grn1_compile_after, nodes = c("SuffHeartF", "CAD"), type = "marginal")
#Conditional 
querygrain(grn1_compile, nodes = c("SuffHeartF", "CAD"), type = "conditional")
querygrain(grn1_compile_after, nodes = c("SuffHeartF", "CAD"), type = "conditional")

####################Partc##################################
sm_25_observation = simulate(grn1_compile_after, nsim = 25)
sm_25_observation
# savefile
setwd("C:/Users/Administrator/W_all_pt1/Desktop/HW4")
write.csv(sm_25_observation, file = "sm_25_observation.csv")
# prediction
pred_CAD_Smoker= predict(grn1_compile_after, response = c("Smoker","CAD"),newdata = sm_25_observation, predictors = c("Sex","SuffHeartF","Hyperchol","Inherit"),type = "class")
#pred_CAD_Smoker
table(pred_CAD_Smoker$pred$Smoker)/length(pred_CAD_Smoker$pred$Smoker)
table(pred_CAD_Smoker$pred$CAD)/length(pred_CAD_Smoker$pred$CAD)
#Marginal from part b without the 25 obervation
querygrain(grn1_compile_after, nodes = c("Smoker", "CAD"), type = "marginal")
####################Partd##################################
sm_500_observation = simulate(grn1_compile_after, nsim = 500)
sm_500_observation
# savefile
setwd("C:/Users/Administrator/W_all_pt1/Desktop/HW4")
write.csv(sm_500_observation, file = "sm_500_observation.csv")
# prediction
pred_CAD_Smoker= predict(grn1_compile_after, response = c("Smoker","CAD"),newdata = sm_500_observation, predictors = c("Sex","SuffHeartF","Hyperchol","Inherit"),type = "class")
#pred_CAD_Smoker
table(pred_CAD_Smoker$pred$Smoker)/length(pred_CAD_Smoker$pred$Smoker)
table(pred_CAD_Smoker$pred$CAD)/length(pred_CAD_Smoker$pred$CAD)
#Marginal from part b without the 500 obervation
querygrain(grn1_compile_after, nodes = c("Smoker", "CAD"), type = "marginal")

