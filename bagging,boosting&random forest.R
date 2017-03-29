dat<-read.csv("dat_withoutNA.csv")

dat$GOOD<-as.factor(dat$GOOD)
dat$CA_01<-as.factor(dat$CA_01)

#Bagging
library(randomForest)
set.seed(6)
bag.fit<-randomForest(GOOD~.,data=dat,mtry=21,importance=TRUE)
pred1<-predict(bag.fit, type="prob")


#Random forest
library(randomForest)
set.seed(6)
rf<-randomForest(GOOD~.,data=dat,mtry=round(sqrt(21)),importance=TRUE)
pred2<-predict(rf,type="prob")

#Boosting 
library(gbm)
set.seed(6)
dat$GOOD<-as.numeric(dat$GOOD)-1
boost.fit<-gbm(GOOD~.,data=dat,distribution="bernoulli",n.trees=5000)
pred3<-predict(boost.fit,n.trees=5000, type="response")

