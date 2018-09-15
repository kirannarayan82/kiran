#Reading the data
mydata<-read.csv(file.choose())

#Lets explore the data
summary(mydata)

#Lets explore the variables types
str(mydata)

names(mydata)

#Lets look at the distribution of each individual variables
attach(mydata)
hist(H_1)#Variable is fairly normally distributed
hist(H_2)#variable is slightly skewed to the right,needs to be approximated to a normal dist
mydata$H_2=(scale(H_2))
hist(H_3)#Variable is fairly normally distributed
hist(H_4)#Variable is fairly normally distributed
hist(H_5)#variable is slightly skewed to the left,needs to be approximated to a normal dist
mydata$H_5=(log(H_5))
hist(H_6)#variable is has multiple peaks,needs to be approximated to a normal dist
mydata$H_6=(sqrt(H_6))
hist(H_7)#Variable is fairly normally distributed
hist(H_8)#Variable is fairly normally distributed
hist(H_9)#Variable is fairly normally distributed
hist(R1)#Variable is fairly normally distributed
hist(R2)#Variable is sligtly skewed to the left
mydata$R2=(log(R2))
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
hist(H_OutSide)   
mydata$H_OutSide=(H_OutSide-mean(H_OutSide))/sd(H_OutSide)
mydata$H_OutSide=scale(mydata$H_OutSide)
hist(Windspeed)
hist(Visibility)
mydata$Visibility=scale(mydata$Visibility)
hist(mydata$Visibility)
#Treatment for variables which are skewed to approximate them to a normal distribute
hist(log(H_5))


hist(R9)



#Check the number of rows and columns



nrow(mydata)
ncol(mydata)

#Here the total consumption is the dependent variables and the remaining columns are the independent variables

modeldata=mydata[,2:25]



#Lets use step regression to identify the significant variables and remove collinearity

lmfit=lm(modeldata$TotalConsmp~.,data=modeldata)
stepb<- step(lmfit, direction="backward")		#backward elimination
stepb$anova					 # display results

stepf<- step(lmfit, direction="forward")		#forward selection
stepf$anova # display results


stepsw<- step(lmfit, direction="both")		#Stepwise selection
stepsw$anova # display results

#From the backward and stepwise selection methods we select R7,R1,R5 as most significant variables which impact the power consumption

#Lets look at the most significant independent variable which are impacting the dependent variable
#Using Random forest regression
library(randomForest)
rfit<-randomForest(modeldata$TotalConsmp~.,data=modeldata)
library(caret)
imp=importance(rfit)
varImpPlot(rfit,type=2)



#Lets look at the most significant variable which are impacting the target variable using gradient boosting
install.packages("gbm")
library(gbm)
gbfit=gbm(modeldata$TotalConsmp ~ . ,data = modeldata,distribution = "gaussian",n.trees = 10000, shrinkage = 0.01, interaction.depth = 4)
summary(gbfit)

#H1,h2,h3,H4 Press_mm-Hg and R3 are the most significant variables from the following 3 models considered above

#Lets split the dataset to train and test

d=sample(nrow(mydata),0.7*nrow(mydata))
train=mydata[d,]
names(train)
test=mydata[-d,]




#Build model using caret package
library(caret)
attach(train)
#Let us control the training parameters using cross validation
fitControl <- trainControl(method = "cv", number = 5, repeats = 5)
model_gbm<-train(train[,2:25],train$TotalConsmp,method='gbm',trControl=fitControl,tuneLength=10)
pred=predict(model_gbm,test)
mean(abs(test$TotalConsmp-pred)/test$TotalConsmp)
print(model_gbm)
plot(model_gbm)
plot(varImp(object=model_gbm),main="GBM - Variable Importance",xlab=c(0,1),ylab=c(0,1))

#Let us control the training parameters using cross validation

control <- trainControl(method="cv", number=5, repeats=3)
seed <- 7
metric <- "RMSE"
set.seed(seed)
mtry <- sqrt(ncol(train))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(train[,2:25],train$TotalConsmp,data=train, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_default)
pred=predict(rf_default,test)
mean(abs(test$TotalConsmp-pred)/test$TotalConsmp)
summary(rf_default)
str(rf_default)
rf_default$finalModel$importance
modelLookup(model='gbm')
print(model_gbm)
plot(model_gbm)
#Calucate mean absolute percentage error between the predicted consumption and the actual consumption 

predict


#Lets look at the most significant variables using adaboost

#Lets check for missing valuessu
summary(is.na(mydata))




#None of the columns have missing values