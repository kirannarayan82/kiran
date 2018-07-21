
#This use case is used to model heart failure risk
mydata<-read.csv(file.choose())
head(mydata)
names(mydata)
attach(mydata)
#convert categorical variables into factors
#Convert ever married to factors
mydata$ever_married<-factor(ever_married,labels=c('0','1'))

#convert Work type to factors
mydata$work_type=factor(work_type,labels=c('1','2','3','4','5'))

#convert residnece type to factors
mydata$Residence_type=factor(Residence_type,labels=c('1','2'))


#convert heart disease into factors
mydata$heart_disease=factor(mydata$heart_disease,labels=c('0','1'))
summary(mydata)

#convert gender into factors

mydata$gender=factor(mydata$gender,labels=c('0','1','2'))

#Impute missing values for bmi
mydata$bmi[is.na(mydata$bmi)]<-median(as.numeric(mydata$bmi),na.rm = TRUE)
mydata$bmi=as.numeric(mydata$bmi)

#impute missing values for smoking_status with the values having highest frequency
summary(mydata$smoking_status)
mydata$smoking_status=factor(mydata$smoking_status,labels=c('1','2','3','4'))


summary(mydata)

#consider the variables needed for modelling
attach(mydata)
modeldata=data.frame(gender,hypertension,heart_disease,ever_married,work_type,Residence_type,avg_glucose_level,bmi,smoking_status,stroke)     
summary(modeldata)
#check for data balance

#check for data balance
table(mydata$stroke)

#data highly imbalanced

library(ROSE)
databal=ovun.sample(stroke~.,data=modeldata,method = "both", N = 43400, seed = 1)$data
#Feature engineering
table(databal$stroke)
library(caret)


library(rpart)
rfit=rpart(databal$stroke~.,data=databal)
varImp(rfit)


library(randomForest)
randomfit=randomForest(as.factor(databal$stroke)~.,data=databal)
randomfit$importance


#selecting the important variables drop gender as it is insignficant for both importance vARIABLE

#spliting the balanced data into train and test
d=sample(nrow(databal),0.7*nrow(databal))
train=databal[d,]
test=databal[-d,]

#training model on naive bayes 
attach(train)
library(e1071)
nbfit=naiveBayes(as.factor(train$stroke)~ever_married+heart_disease+avg_glucose_level+work_type+bmi+smoking_status+Residence_type+hypertension,data=train)
table(test$stroke,predict(nbfit,test))


#training model on svm


svfitt=svm(as.factor(train$stroke)~ever_married+heart_disease+avg_glucose_level+work_type+bmi+smoking_status+Residence_type+hypertension,data=train)
table(test$stroke,predict(svfitt,test))

#training model on randomForest
rfitt=randomForest(as.factor(train$stroke)~ever_married+heart_disease+avg_glucose_level+work_type+bmi+smoking_status+Residence_type+hypertension,data=train)
table(test$stroke,predict(rfitt,test))

#random forest is giving us the highest precision
#load the scoring data

finaldata<-read.csv(file.choose())

#Apply the same treatment on the final scoring data as the orginal data
mydata=finaldata
#convert categorical variables into factors
#Convert ever married to factors
mydata$ever_married<-factor(ever_married,labels=c('0','1'))

#convert Work type to factors
mydata$work_type=factor(work_type,labels=c('1','2','3','4','5'))

#convert residnece type to factors
mydata$Residence_type=factor(Residence_type,labels=c('1','2'))


#convert heart disease into factors
mydata$heart_disease=factor(mydata$heart_disease,labels=c('0','1'))
summary(mydata)

#convert gender into factors

mydata$gender=factor(mydata$gender,labels=c('0','1','2'))

#Impute missing values for bmi
mydata$bmi[is.na(mydata$bmi)]<-median(as.numeric(mydata$bmi),na.rm = TRUE)
mydata$bmi=as.numeric(mydata$bmi)

#impute missing values for smoking_status with the values having highest frequency
summary(mydata$smoking_status)
mydata$smoking_status=factor(mydata$smoking_status,labels=c('1','2','3','4'))

finaldata=mydata
#select the same set of variables on which the model is built on
names(mydata)
nrow(mydata)
names(mydata)
attach(mydata)
finaldata=data.frame(ever_married,heart_disease,avg_glucose_level,work_type)
nrow(finaldata)

#Final predictions on random forest
finalpredict=predict(rffit,finaldata)
nrow(data.frame(finalpredict))
predictresults=data.frame(mydata$id,data.frame(finalpredict))

write.csv(predictresults,'D:/finalpredictions.csv')
