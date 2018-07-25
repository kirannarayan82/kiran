#Kaggle bad buy use case
#Machine  Learning model to identify bad car purchases
newdata<-read.csv(file.choose())
newdata
head(newdata)
summary(newdata)

#columns PRIMEUNIT And AUCGUART are almost completely null they can be completely removedcolnam
colnames<-(names(newdata) %in% c("PRIMEUNIT","AUCGUART"))          
newdata=newdata[,!(names(newdata) %in% c("PRIMEUNIT","AUCGUART"))]
names(newdata)
summary(newdata)
newdata<-na.omit(newdata)
newdata<-newdata[newdata==NULL,]

#exploringthe data to understand what drives bad buys

library(ggplot2)
ggplot(numdata,aes(x=newdata$Make,y=as.factor(newdata$IsBadBuy),fill=as.factor(newdata$IsBadBuy)))+geom_bar(stat="identity")
ggplot(newdata,aes(x=newdata$SubModel,y=as.factor(newdata$IsBadBuy),fill=as.factor(newdata$IsBadBuy)))+geom_bar(stat="identity")
ggplot(newdata,aes(x=newdata$Color,y=as.factor(newdata$IsBadBuy),fill=as.factor(newdata$IsBadBuy)))+geom_bar(stat="identity")
ggplot(newdata,aes(x=newdata$Transmission,y=as.factor(newdata$IsBadBuy),fill=as.factor(newdata$IsBadBuy)))+geom_bar(stat="identity")
ggplot(newdata,aes(x=newdata$WheelType,y=as.factor(newdata$IsBadBuy),fill=as.factor(newdata$IsBadBuy)))+geom_bar(stat="identity")
ggplot(newdata,aes(x=newdata$Nationality,y=as.factor(newdata$IsBadBuy),fill=as.factor(newdata$IsBadBuy)))+geom_bar(stat="identity")

#treat remaining missing values with medians
attach(newdata)
newdata[is.na(MMRAcquisitionAuctionAveragePrice)]<-median(as.numeric(newdata$MMRAcquisitionAuctionAveragePrice),na.rm=TRUE)
newdata[is.na(MMRAcquisitionRetailAveragePrice)]<-median(as.numeric(newdata$MMRAcquisitionRetailAveragePrice),na.rm=TRUE)
newdata[is.na(MMRAcquisitonRetailCleanPrice)]<-median(as.numeric(MMRAcquisitonRetailCleanPrice),na.rm=TRUE)
newdata[is.na(MMRCurrentAuctionAveragePrice)]<-median(as.numeric(MMRCurrentAuctionAveragePrice),na.rm=TRUE)
newdata[is.na(MMRCurrentRetailCleanPrice)]<-median(as.numeric(newdata$MMRCurrentRetailCleanPrice),na.rm=TRUE)


#exploring the data to understand which are the cars which are faulty
with(newdata,table( IsBadBuy,Size ))
with(newdata,table( IsBadBuy,VehicleAge ))
with(newdata,table( IsBadBuy,Make ))
with(newdata,table(IsBadBuy,Size))


#Remove Remaining null values
newdata<-na.omit(newdata)


summary(newdata)

names(newdata)
attach(newdata)

library(randomForest)


#split the data into training and test set
d<-sample(nrow(newdata),0.7*nrow(newdata))
train<-newdata[d,]

test<-newdata[-d,]

#check for data balance
table(newdata$IsBadBuy)

#Data is heavily in terms of majority cases being of a good buy this can be handled with wieights  in the random forest algorithm

attach(newdata)                              


fit<-randomForest(as.factor(IsBadBuy)~Make+Transmission+VehOdo+as.numeric(MMRAcquisitionAuctionAveragePrice)+as.numeric(MMRAcquisitionAuctionCleanPrice)+as.numeric(MMRAcquisitionRetailAveragePrice)+as.numeric(MMRAcquisitonRetailCleanPrice)+as.numeric(MMRCurrentAuctionAveragePrice)+as.numeric(MMRCurrentAuctionCleanPrice)+as.numeric(MMRCurrentRetailAveragePrice)+as.numeric(MMRCurrentRetailCleanPrice)+VehBCost+IsOnlineSale+WarrantyCost,data=train,Classwt=c(0.3,0.7))
fit$importance
attach(train)
#Predict on test data and check for accuracy
pred=predict(fit,test)
table(pred,test$IsBadBuy)
install.packages("pROC")
plot(roc(pred,test$IsBadBuy, direction="<"),col="yellow", lwd=3, main="ROC curve")
simp_roc <- roc(pred,test$IsBadBuy)
auc(simp_roc)
Area under the curve: 0.8638

#check for correlcations
numdata<-as.numeric(MMRAcquisitionAuctionAveragePrice)+as.numeric(MMRAcquisitionAuctionCleanPrice)+as.numeric(MMRAcquisitionRetailAveragePrice)+as.numeric(MMRAcquisitonRetailCleanPrice)+as.numeric(MMRCurrentAuctionAveragePrice)+as.numeric(MMRCurrentAuctionCleanPrice)+as.numeric(MMRCurrentRetailAveragePrice)+as.numeric(MMRCurrentRetailCleanPrice)
numdata<-data.frame(as.numeric(MMRAcquisitionAuctionAveragePrice),as.numeric(MMRAcquisitionAuctionCleanPrice),as.numeric(MMRAcquisitionRetailAveragePrice),as.numeric(MMRAcquisitonRetailCleanPrice),as.numeric(MMRCurrentAuctionAveragePrice),as.numeric(MMRCurrentAuctionCleanPrice),as.numeric(MMRCurrentRetailAveragePrice),as.numeric(MMRCurrentRetailCleanPrice))
cor(numdata)

#drop correlated variables and check for accuracy
fit2<-randomForest(as.factor(IsBadBuy)~Make+Transmission+VehOdo+as.numeric(MMRCurrentRetailCleanPrice)+VehBCost+IsOnlineSale+WarrantyCost,data=train,Classwt=c(0.7,0.5))
pred2=predict(fit2,test)
simp_roc <- roc(pred2,test$IsBadBuy)
auc(simp_roc)
Area under the curve: 0.7113
plot(roc(pred2,test$IsBadBuy, direction="<"),col="green", lwd=3, main="ROC curve")

