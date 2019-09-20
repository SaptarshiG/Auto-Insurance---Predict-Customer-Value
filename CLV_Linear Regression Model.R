setwd("C:\\Users\\ezgucsa\\Desktop\\IVY Class Notes\\R\\R Project")
autoinsurance<-read.csv("Fn-UseC_-Marketing-Customer-Value-Analysis.csv")
View(autoinsurance)

#Calling required Libraries
library(car)
library(lmtest)
library(nortest)
library(dplyr)
library(stringr)



#DIY Data Exploration - Check Data Structure, and Summary
str(autoinsurance)
summary(autoinsurance)
autoinsurance$Income=as.numeric(as.character(autoinsurance$Income))
autoinsurance =autoinsurance[(rowSums(is.na(autoinsurance)) <= 2),]

#Cleaning up the column names
colnames(autoinsurance)
colnames(autoinsurance)<-str_replace_all(colnames(autoinsurance),"[:punct:]","")


#Eploratory Data Analysis
autoinsurance$NumberofOpenComplaints[is.na(autoinsurance$NumberofOpenComplaints)]<-0



#Cleaning up the dataset
#Checking for missing values are available or not in all columns
library(summarytools)
dfSummary(autoinsurance)#No missing values are there


#Checking for outliers
boxplot(autoinsurance$Income)
boxplot(autoinsurance$MonthlyPremiumAuto)#Outliers treatment needed
boxplot(autoinsurance$MonthsSinceLastClaim)
boxplot(autoinsurance$MonthsSincePolicyInception)
boxplot(autoinsurance$TotalClaimAmount)#Outliers treatment needed
boxplot(autoinsurance$CustomerLifetimeValue)#Outliers treatment needed



library(ggplot2)

# Inorder to check the means of the various coverages and imputing based on that.
ggplot(data=autoinsurance,aes(Coverage,MonthlyPremiumAuto,colour="rainbow"))+geom_boxplot(na.rm = T)
ggplot(data=autoinsurance,aes(MonthlyPremiumAuto,colour="rainbow"))+geom_density(na.rm = T)

autoinsurance$Coverage=as.factor(autoinsurance$Coverage)
levels(autoinsurance$Coverage)=c("Basic","Extended","Premium","Basic","Extended","Premium")
summary(autoinsurance)
levels(autoinsurance$Coverage)

ggplot(data=autoinsurance,aes(Income,colour="rainbow"))+geom_density(na.rm = T)+facet_wrap(~Education)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




### Knn Imputation in order to impute the Na values
#install.packages("DMwR")
library(DMwR)
autoinsurance=knnImputation(autoinsurance,k=5)


sum(is.na(autoinsurance))
cor(autoinsurance$CustomerLifetimeValue,autoinsurance$MonthlyPremiumAuto)
cor(autoinsurance$CustomerLifetimeValue,autoinsurance$MonthsSincePolicyInception)




## Making vehicle size as factor
autoinsurance$VehicleSize=as.factor(autoinsurance$VehicleSize)
levels(autoinsurance$VehicleSize)

#Outliers Treatment
num=autoinsurance[sapply(autoinsurance,is.numeric)]
library(ggplot2)
ggplot(data = num,aes(x="",y=CustomerLifetimeValue,col="rainbow"))+geom_boxplot() # Greater than 50000
autoinsurance=autoinsurance[autoinsurance$CustomerLifetimeValue<50000,]

ggplot(data = num,aes(x="",y=Income,col="rainbow"))+geom_boxplot()
ggplot(data = num,aes(x="",y=MonthlyPremiumAuto,col="rainbow"))+geom_boxplot() # greater than 160
#autoinsurance=autoinsurance[autoinsurance$MonthlyPremiumAuto<160,]

ggplot(data = num,aes(x="",y=MonthsSinceLastClaim,col="rainbow"))+geom_boxplot()
ggplot(data = num,aes(x="",y=MonthsSincePolicyInception,col="rainbow"))+geom_boxplot()
ggplot(data = num,aes(x="",y=NumberofOpenComplaints,col="rainbow"))+geom_boxplot()#removing outliers not required
ggplot(data = num,aes(x="",y=NumberofPolicies,col="rainbow"))+geom_boxplot()#removing outliers not required
ggplot(data = num,aes(x="",y=TotalClaimAmount,col="rainbow"))+geom_boxplot() # Greater than 900 claim amount remove it.
#autoinsurance=autoinsurance[autoinsurance$Total.Claim.Amount<900,]


#Spliting dataset in train & test based on outcome: 70% & 30%
library(caret)
index<-createDataPartition(autoinsurance$CustomerLifetimeValue, p=0.70, list = FALSE)
trainset<-autoinsurance[index,]
testset<-autoinsurance[-index,]
nrow(trainset)
nrow(testset)


#The Model
set.seed(123)


#Regression in Train data
colnames(trainset)
CustomerID1<-trainset[,1]#Storing Customer ID in a object
View(CustomerID1)
trainset<-trainset[,-c(1)]#Customer column not needed
Reg<-lm(1/(CustomerLifetimeValue)~., data = trainset)
#install.packages("MASS")
library(MASS)
stepAIC(Reg)
Reg1<-lm(1/(CustomerLifetimeValue)~Coverage + EmploymentStatus + Gender + Income + MaritalStatus + MonthlyPremiumAuto + NumberofOpenComplaints + NumberofPolicies + RenewOfferType + VehicleClass, data = trainset)
summary(Reg)
summary(Reg1)

vif(Reg1)


#Get fitted predicted value
fitted(Reg1)


#Checking for MAPE
trainset$pred<-(1/fitted(Reg1))
View(trainset)
trainset$Error<- abs((trainset$CustomerLifetimeValue-trainset$pred)
                      /(trainset$CustomerLifetimeValue)*100)
mean(trainset$Error,na.rm = TRUE)


#DurwinWatson Test to check Auto-correlation
dwt(Reg1)#No Auto-correlation exists


#Breush-Pegan Test to Check homoscedasticity
bptest(Reg1)#residuals are not constant

#Normality Test of Residuls by Anderson-Darling Test/Shapiro-Wilk Normality Test

resids<-Reg1$residuals
ad.test(resids)#residuals are not normal


#Regression in Test data
colnames(testset)
CustomerID2<-testset[,1]#Storing Customer ID in a object
View(CustomerID2)
testset<-testset[,-c(1)]#Customer column not needed

Reg2<-lm(1/(CustomerLifetimeValue)~Coverage + EmploymentStatus + Gender + Income + MaritalStatus + MonthlyPremiumAuto + NumberofOpenComplaints + NumberofPolicies + RenewOfferType + VehicleClass, data = testset)
summary(Reg2)


#Checking for MAPE in Test data
testset$pred<-(1/fitted(Reg2))
View(testset)
testset$Error<- abs((testset$CustomerLifetimeValue-testset$pred)
                     /(testset$CustomerLifetimeValue)*100)
mean(testset$Error,na.rm = TRUE)

trainset1<-cbind(CustomerID1,trainset)
colnames(trainset1)
testset2<-cbind(CustomerID2,testset)
colnames(testset2)


write.csv(trainset1,"trainmodelledset.csv",row.names = FALSE)
write.csv(testset2,"testmodelledset.csv",row.names = FALSE)

