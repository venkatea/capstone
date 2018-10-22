#######################################################################################
#Capstone Project
#######################################################################################
#Install Libraries - if needed

install.packages('ROCR')
install.packages('randomForest')
install.packages('neuralnet')
install.packages('e1071')
install.packages('pROC')
install.packages('epiR')

#Import Libraries

library(ROCR)
library(randomForest)
library(neuralnet)
library(e1071)
library(pROC)
library(epiR)

#Setup Dependent Variable - import ProcessedLeadData dataset

LeadData <- LeadDataFinal

#Feature Create Op based on if Opportunity

LeadData$Op <- 1
LeadData$Op[LeadData$Opportunity..Created.Date == ""] <- 0


#Check to see if feature create was successful

table(LeadData$Op)

#Split into Train and Test

set.seed(10)
indexes = sample(1:nrow(LeadData), size=0.2*nrow(LeadData))
Test = LeadData[indexes,]
Train = LeadData[-indexes,]

#######################################################################################
##Unbalanced Test
#######################################################################################
#Logistical Regression - Unbalanced Class

LRmodelu <- glm(Op ~ EmployeeSize1100 + EmployeeSize1001k + EmployeeSize1k10k + EmployeeSize10k50k + EmployeeSize50k100k + EmployeeSize100k200k + EmployeeSize200k + board + chief + director + head + consultant + business + student + technical + Pardot.Score + Count.in.lead + Count.in.Campaign,data=Train,family=binomial)
summary(LRmodelu)

#Predictions based on Logistical Regression including ROC Curve

LRpredictu <- predict(LRmodelu,newdata=Test)
LRpredictu[LRpredictu>0.5] <- 1
LRpredictu[LRpredictu<0.5] <- 0
LRroccurveu <- roc(Test$Op ~ LRpredictu)
plot(LRroccurveu)
LRAUCu <- auc(LRroccurveu)

#Create table for prediction, conver to confusion matrix
table1 <- table(Test$Op,LRpredictu)
a <- 0
b <- 0
c <- 0
d <- 0
a <- table1[1,1]
b <- table1[1,2]
c <- table1[2,1]
d <- table1[2,2]

ConfusionMatrix <- as.table(matrix(c(d,c,b,a),nrow=2,byrow=T))
table3 <- as.table(matrix(c(d,b,c,a),nrow=2,byrow=T))

#Calculate Sensitivity/Specificity

epi.tests(table3)

########################################################################################
#Balance Classes/Enrichment for Train set only
########################################################################################
prevalence <- .8
N <- 50000

tset <- Train[Train$Op==1,]
other <- Train[Train$Op==0,]

ntarget <- round(N*prevalence)

set.seed(575)
heads<-tset
heads<-heads[sample(nrow(heads),ntarget,replace=T),]
tails<-other
tails<-tails[sample(nrow(tails),N-ntarget,replace=T),]

Lead <- rbind(heads,tails)
########################################################################################
#Logistical Regression - Balanced Class
########################################################################################
LRmodel <- glm(Op ~ EmployeeSize1100 + EmployeeSize1001k + EmployeeSize1k10k + EmployeeSize10k50k + EmployeeSize50k100k + EmployeeSize100k200k + EmployeeSize200k + board + chief + director + head + consultant + business + student + technical + Pardot.Score + Count.in.lead + Count.in.Campaign,data=Lead,family=binomial)
summary(LRmodel)

#Predictions based on Logistical Regression including ROC Curve

LRpredict <- predict(LRmodel,newdata=Test)
LRpredict[LRpredict>0.1] <- 1
LRpredict[LRpredict<0.1] <- 0
LRroccurve <- roc(Test$Op ~ LRpredict)
plot(LRroccurve)
LRAUC <- auc(LRroccurve)

#Create table for prediction, conver to confusion matrix
table1 <- table(Test$Op,LRpredict)
a <- 0
b <- 0
c <- 0
d <- 0
a <- table1[1,1]
b <- table1[1,2]
c <- table1[2,1]
d <- table1[2,2]
ConfusionMatrix <- as.table(matrix(c(d,c,b,a),nrow=2,byrow=T))
table3 <- as.table(matrix(c(d,b,c,a),nrow=2,byrow=T))

#Calculate Sensitivity/Specificity

epi.tests(table3)

#######################################################################################
#Random Forest
#######################################################################################
#Set Seed
set.seed(100)

#Fit Model to Random Forest Algorithm
fit <- randomForest(Op ~ EmployeeSize1100 + EmployeeSize1001k + EmployeeSize1k10k + EmployeeSize10k50k + EmployeeSize50k100k + EmployeeSize100k200k + EmployeeSize200k + board + chief + director + head + consultant + business + student + technical + Pardot.Score + Count.in.lead + Count.in.Campaign,data=Lead, importance=TRUE, ntree=20)
varImpPlot(fit)

#Predictions based on fit
RFPrediction <- predict(fit, newdata=Test)
RFPrediction[RFPrediction>.5] <- 1
RFPrediction[RFPrediction<.5] <- 0

#Generate ROC Curve
RFroccurve <- roc(Test$Op ~ RFPrediction)
plot(RFroccurve)
RFAUC <- auc(RFroccurve)

#Create table for prediction, conver to confusion matrix
table1 <- table(Test$Op,RFPrediction)
a <- table1[1,1]
b <- table1[1,2]
c <- table1[2,1]
d <- table1[2,2]

ConfusionMatrix <- as.table(matrix(c(d,c,b,a),nrow=2,byrow=T))
table3 <- as.table(matrix(c(d,b,c,a),nrow=2,byrow=T))

#Calculate Sensitivity/Specificity

cf <- cforest(Op ~ EmployeeSize1100 + EmployeeSize1001k + EmployeeSize1k10k + EmployeeSize10k50k + EmployeeSize50k100k + EmployeeSize100k200k + EmployeeSize200k + board + chief + director + head + consultant + business + student + technical + Pardot.Score + Count.in.lead + Count.in.Campaign,data=Lead, controls=cforest_control(mtry=2, mincriterion=0))
randomForest::varImpPlot(fit)
####################################################################################
#Support Vector Machines
####################################################################################

SVMmodel <- svm(Op ~ EmployeeSize1100 + EmployeeSize1001k + EmployeeSize1k10k + EmployeeSize10k50k + EmployeeSize50k100k + EmployeeSize100k200k + EmployeeSize200k + board + chief + director + head + consultant + business + student + technical + Pardot.Score + Count.in.lead + Count.in.Campaign, data=Lead)

#Predict based on Model
SVMPredict <- predict(SVMmodel,Test)
SVMPredict[SVMPredict>.5] <- 1
SVMPredict[SVMPredict<.5] <- 0
#Plot ROC Curve
SVMroccurve <- roc(Test$Op ~ SVMPredict)
plot(SVMroccurve)
SVMAUC <- auc(SVMroccurve)

#Create table for prediction, conver to confusion matrix
table1 <- table(Test$Op,LRpredictu)
a <- 0
b <- 0
c <- 0
d <- 0
a <- table1[1,1]
b <- table1[1,2]
c <- table1[2,1]
d <- table1[2,2]

ConfusionMatrix <- as.table(matrix(c(d,c,b,a),nrow=2,byrow=T))
table3 <- as.table(matrix(c(d,b,c,a),nrow=2,byrow=T))

#Calculate Sensitivity/Specificity

epi.tests(table3)

