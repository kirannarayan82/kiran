#Sample Loan Propensity Modelling

#Load the training data

loandata<-read.csv(file.choose(),na.strings = c("", "NA"))
#Lets explore the data
summary(loandata)

#Look at the structure of the dataset

str(loandata)

#Check each value of missing variable
summary(loandata$City_Category)

summary(loandata$City_Category)

#City category has 814 missing values, we can replace them with the most frequently occuring  city category "A"

loandata$City_Category[which(is.na(loandata$City_Category))]<-"A"



summary(loandata$Employer_Category1)

# Employer category1 has 4018 missing value, Replacing this with most frequently occuring employee cateogory "A"
loandata$Employer_Category1[which(is.na(loandata$Employer_Category1))]<-"A"
#employer category 2 is shown as type of numeric ,it must be converted to a factor and its missing values also has to be treated
loandata$Employer_Category2<-as.factor(loandata$Employer_Category2)

summary(loandata$Employer_Category2)

#employer category 2 has 4298 missing values which can be replaced by the most frequently occuring category =4

loandata$Employer_Category2[which(is.na(loandata$Employer_Category2))]<-4

#Primary_bank_type has 9391 missing values which needs to be treated as well with the most frequently occuring Primary bank type P

loandata$Primary_Bank_Type[which(is.na(loandata$Primary_Bank_Type))]<-"P"

#Replace missing values in existing EMI with the median values
loandata$Existing_EMI[which(is.na(loandata$Existing_EMI))]<-median(loandata$Existing_EMI,na.rm=TRUE)

#Replace missing values in Loan Amount with median values
loandata$Loan_Amount[which(is.na(loandata$Loan_Amount))]<-median(loandata$Loan_Amount,na.rm=TRUE)


#Replace missing values in Loan Period with median values
loandata$Loan_Period[which(is.na(loandata$Loan_Period))]<-median(loandata$Loan_Period,na.rm=TRUE)

#Replace missing values interest rates with median values
loandata$Interest_Rate[which(is.na(loandata$Interest_Rate))]<-median(loandata$Interest_Rate,na.rm=TRUE)

#Replace missing values  EMI with median values
loandata$EMI[which(is.na(loandata$EMI))]<-median(loandata$EMI,na.rm=TRUE)


summary(loandata)

#We will not use variables which denote ids and codes, selecting the other variables which will be used in our modelling
frame<-with(loandata,data.frame(City_Category,Employer_Category1,Employer_Category2,Monthly_Income,Primary_Bank_Type,Contacted,Existing_EMI,Loan_Amount,Loan_Period,Interest_Rate,EMI,Approved))
summary(frame)

#Feature Engineering using Random Forest
attach(frame)
library(randomForest)
model_rf<-randomForest(as.factor(Approved)~.,data=frame)
summary(model_rf)
model_rf$importance

#Feature engineering using logistic regression

model_glm <- glm(as.factor(Approved)~.,data=frame,family="binomial")
#Check the significant variables based on p values
summary(model_glm)

#From both the feature engineering methods above we choose the following independent vairbales
#Monthly_Income
#Existing_EMI 
##Loan_Amount 
#Loan_Period
#EMI  

#selecting the final list of variables in a finaldataframe

finalframe<-with(frame,data.frame(Monthly_Income,Existing_EMI,Loan_Amount,Loan_Period,EMI,Approved))

#check for correlations in training set
cor(train)

#EMI looks to be highky correlated with Loan amount, so we will drop EMI


finalframe<-with(frame,data.frame(Monthly_Income,Existing_EMI,Loan_Amount,Loan_Period,Approved))

#check data balance
table(finalframe$Approved)


#data is heavily unbalanced in favour of 0 or no approval

#diving the final frame into test and train
d=sample(nrow(finalframe),nrow(finalframe)*0.70)

train=finalframe[d,]
test=finalframe[-d,]

#Fitting a random forest model with class weights 50% to both cases
rf_fit=with(finalframe,randomForest(as.factor(Approved)~Monthly_Income+Existing_EMI+Loan_Amount+Loan_Period,data=finalframe,classwt=c(0.5,0.5)))

table(predict(rf_fit,train))

table(predict(rf_fit,test))

table(test$Approved,predict(rf_fit,test))
predictions<-predict(rf_fit,test)


install.packages("pROC")
library(pROC)
auc(test$Approved,as.numeric(predictions))

#Area under the curve is almost 0.94


#We will try to see if we can use other algorithms to improve accuracy 
#We will try by using oversampling the minority class. We had 1020 minority class observations out of a total of 69713 observations. We will oversample the minority class till we have 69713 observations
install.packages("ROSE")
library(ROSE)

data_balanced_over <- ovun.sample(as.factor(Approved)~Monthly_Income+Existing_EMI+Loan_Amount+Loan_Period, data = finalframe, method = "over",N = 69713)$data

#Using Naive bayes to train and predict

library(naivebayes)
model_nb=naiveBayes(as.factor(Approved)~Monthly_Income+Existing_EMI+Loan_Amount+Loan_Period,data=data_balanced_over)


predictions<-predict(model_nb,test)
table(test$Approved,predict(model_nb,test))
predictions<-predict(model_nb,test)
library(pROC)
auc(test$Approved,as.numeric(predictions))

#Naive baiyes  Area under curve 0.54


#using svm to train and predict


#Using support vector to train and predict

library(e1071)
model_svm=svm(as.factor(Approved)~Monthly_Income+Existing_EMI+Loan_Amount+Loan_Period,data=data_balanced_over)


predictions<-predict(model_svm,test)
table(test$Approved,predict(model_svm,test))
predictions<-predict(model_svm,test)
library(pROC)
auc(test$Approved,as.numeric(predictions))

#svm area under curve around 0.5 

#From the above 3 models we choose random forest which has the highest Area under curve to score the final results

#Loading the final scoring set

loandata_score<-read.csv(file.choose(),na.strings = c("", "NA"))

#selecting the relevant columns in the scoring set
#selecting the final list of variables in a finaldataframe
summary(loandata_score)
str(test)
str(predictions)




finalscore<-with(loandata_score,data.frame(Monthly_Income,Existing_EMI,Loan_Amount,Loan_Period,EMI))



names(finalscore)


#some independent variables have missing values  in few observations which cannot generate predictions,such cases will be treated with medians

#Replace missing values in existing EMI with the median values
finalscore$Existing_EMI[which(is.na(finalscoredata$Existing_EMI))]<-median(finalscoredata$Existing_EMI,na.rm=TRUE)

#Replace missing values in Loan Amount with median values
finalscore$Loan_Amount[which(is.na(finalscoredata$Loan_Amount))]<-median(finalscoredata$Loan_Amount,na.rm=TRUE)


#Replace missing values in Loan Period with median values
finalscore$Loan_Period[which(is.na(finalscoredata$Loan_Period))]<-median(finalscoredata$Loan_Period,na.rm=TRUE)

#Replace missing values interest rates with median values
finalscore$Interest_Rate[which(is.na(finalscoredata$Interest_Rate))]<-median(finalscoredata$Interest_Rate,na.rm=TRUE)

#Replace missing values  EMI with median values
finalscore$EMI[which(is.na(finalscoredata$EMI))]<-median(finalscoredata$EMI,na.rm=TRUE)


finalscore$Monthly_Income[which(is.na(finalscore$Monthly_Income))]<-median(finalscore$Monthly_Income,na.rm=TRUE)

summary(finalscore)

#generate finalpredictions on the scoring set using the random forests method
Approved<-predict(rf_fit,finalscore)

finalscore=data.frame(finalscore,Approved)

summary(finalscore)

#Load the final predictions into a Csv file

write.csv(data.frame(loandata_score$ID,finalscore$Approved),"C:/Users/genus/Documents/kaggle/Loan/Finalsubmission.csv")

