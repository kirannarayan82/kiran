#Reading the data
mydata<-read.csv(file.choose())

#Lets explore the data
summary(mydata)

#Lets explore the variables types
str(mydata)

names(mydata)

#check for missing values

summary(is.na(mydata))

#Lets look at the distribution of each individual variables
attach(mydata)
hist(H_1)#Variable is fairly normally distributed
hist(H_2)#variable is slightly skewed to the right,needs to be approximated to a normal dist
mydata$H_2=(scale(H_2))
hist(mydata$H_2)
hist(H_3)#Variable is fairly normally distributed
hist(H_4)#Variable is fairly normally distributed
hist(H_5)#variable is slightly skewed to the left,needs to be approximated to a normal dist
mydata$H_5=(log(H_5))
hist(mydata$H_5)
hist(H_6)#variable is has multiple peaks,needs to be approximated to a normal dist
mydata$H_6=(sqrt(H_6))
mydata$H_6=mydata$H_6-mean(mydata$H_6)/sd(mydata$H_6)
hist(mydata$H_6)
mydata$H_6=cut(mydata$H_6,3,labels=c('1','2','3'))
hist(mydata$H_6)
hist(H_7)#Variable is fairly normally distributed
hist(H_8)#Variable is fairly normally distributed
hist(H_9)#Variable is fairly normally distributed
hist(R1)#Variable is fairly normally distributed
hist(R2)#Variable is sligtly skewed to the left
mydata$R2=(log(R2))
hist(mydata$R2)
hist(R3)#Variable is fairly normally distributed
hist(R4)#Variable is fairly normally distributed
hist(R5)#Variable is fairly normally distributed
hist(R6)#Variable is fairly normally distributed
hist(R7)#variable is has multiple peaks,needs to be approximated to a normal dist
mydata$R7=ifelse(R7>(median(R7)+3*sd(R7)),median(R7),ifelse(R7<(median(R7)-3*sd(R7)),median(R7),R7))
hist(R7)#Now this variable is fairly normally distributed
hist(R8)#Variable is fairly normally distributed
hist(TempOutSide) #Variable is fairly normally distributed
hist(Press_mm_hg)#variable is highly skewed
mydata$Press_mm_hg=ifelse(Press_mm_hg>(median(Press_mm_hg)+3*sd(Press_mm_hg)),median(Press_mm_hg),ifelse(Press_mm_hg<(median(Press_mm_hg)-3*sd(Press_mm_hg)),median(Press_mm_hg),Press_mm_hg))
hist(mydata$Press_mm_hg)
hist(H_OutSide)   

mydata$H_OutSide=sqrt(mydata$H_OutSide)
mydata$H_OutSide=log(mydata$H_OutSide)
hist(mydata$H_OutSide)
mydata$H_OutSide=cut(mydata$H_OutSide,3,labels=c('1','2','3'))
hist(Windspeed)
mydata$Windspeed=(Windspeed-mean(Windspeed))/sd(Windspeed)
hist(mydata$Windspeed)
mydata$Windspeed=cut(mydata$Windspeed,3,labels=c('1','2','3'))
hist(Visibility)

hist(mydata$Visibility)


str(mydata)





names(mydata)


#Build model using caret package
library(caret)

names(train)

#Check for correlations on the numeric variables
correlationMatrix <- cor(mydata[,c(3:12,13,15:22,25)])
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)
highlyCorrelated=paste(highlyCorrelated,sep=",")
stringr::str_replace(highlyCorrelated," ",",")
drop=c(12,13,5,17,19,9,3,1,11,15,8,2,14,6,16,18)
trainred=mydata[,-c(drop)]
names(trainred)
modeldata=data.frame(trainred,mydata$TotalConsmp)
names(modeldata)


#Lets split the dataset to train and test


d=sample(nrow(modeldata),0.7*nrow(modeldata))
train=modeldata[d,]
names(train)
test=modeldata[-d,]

names(train)=c("H_1","R3","H_4","H_9","TempOutSide","Press_mm_hg","H_OutSide","Windspeed","Visibility","TotalConsmp")
names(test)=c("H_1","R3","H_4","H_9","TempOutSide","Press_mm_hg","H_OutSide","Windspeed","Visibility","TotalConsmp")

library(ggplot2)
ggplot(train,aes(x=train$H_1, y=train$TotalConsmp)) +geom_point(shape=1) + geom_smooth(method=lm) 
ggplot(train,aes(x=train$H_4, y=train$TotalConsmp)) +geom_point(shape=1) + geom_smooth(method=lm) 
ggplot(train,aes(x=train$H_9, y=train$TotalConsmp)) +geom_point(shape=1) + geom_smooth(method=lm) 
ggplot(train,aes(x=train$R3, y=train$TotalConsmp)) +geom_point(shape=1) + geom_smooth(method=lm) 
ggplot(train,aes(x=train$Press_mm_hg, y=train$TotalConsmp)) +geom_point(shape=1) + geom_smooth(method=lm) 
ggplot(train,aes(x=train$H_OutSide, y=train$TotalConsmp)) + geom_boxplot()
ggplot(train,aes(x=train$Windspeed, y=train$TotalConsmp)) +geom_boxplot()
ggplot(train,aes(x=train$Visibility, y=train$TotalConsmp)) +geom_point(shape=1) + geom_smooth(method=lm) 

#No linear relationship between dependent and independent variables, we will mainly used treee based models and kmeans neighbours


1
2
3s
4
5
6
7
8
9




fitControl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5)

model_gbm<-train(train[,1:9],train$TotalConsmp,method='gbm',trControl=fitControl,tuneLength=10)
plot(varImp(model_gbm))
pred=predict(model_gbm,test)
mean(abs(test$TotalConsmp-pred)/test$TotalConsmp)
plot(varImp(model_gbm))


names(train)
str(train)

control <- trainControl(method="cv", number=5, repeats=3)
seed <- 7
metric <- "RMSE"
treemodel <- train(train[,1:9],train$TotalConsmp,method="rpart",trControl=control)

pred=predict(treemodel,test)
mean(abs(test$TotalConsmp-pred)/test$TotalConsmp)
plot(varImp(treemodel))
str(train)

#Predict using knn, the categorical variables need to be converted to dummy variables for modeling

install.packages("dummies")
library(dummies)
knntrain=train
knntest=test
knntrain$H_OutSide=dummy(train$H_OutSide)
knntrain$Windspeed=dummy(train$Windspeed)
knntest$H_OutSide=dummy(test$H_OutSide)
knntest$Windspeed=dummy(test$Windspeed)
knnmodel <- train(train[,1:9],train$TotalConsmp,method="knn",trControl=control)
pred=predict(knnmodel,test)
mean(abs(test$TotalConsmp-pred)/test$TotalConsmp)
plot(varImp(knnmodel))


library(e1071)
svmmodel=svm(train$TotalConsmp~.,data=train)
pred=predict(svmmodel,test)
mean(abs(test$TotalConsmp-pred)/test$TotalConsmp)

plot(varImp(object=svmmodel),main="GBM - Variable Importance",xlab=c(0,1),ylab=c(0,1))


