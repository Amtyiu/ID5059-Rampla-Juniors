# ID 5059 

# Neural Network

library(nnet)
library(fields)
library(rgl)
library(caret)
library(neuralnet)
library(NeuralNetTools)

data1<-read.csv("dat_withoutNA.csv",header=T)
attach(data1)

data1$CA_01<-as.factor(data1$CA_01)

data1$GOOD<-as.factor(data1$GOOD)

str(data1)

detach(dataDM4)

dataDM<-read.csv("Imp3_id.csv",header=T)
dataDM1<-dataDM[,-1]
dataDM2<-dataDM[dataDM1$id=="dm"&dataDM1$app_id>0,]
dataDM3<-dataDM2[,-c(1,28,29)]
str(dataDM3)

dataHold<-dataDM[dataDM1$id=="hold",]
dataHold1<-read.csv("ALG_HOLDOUT_WITHOUT_OUTCOME.csv",header=T)
identical(dataHold$app_id,dataHold1$app_id)

dataHold2<-dataHold[,-1]
dataHold3<-dataHold2[,-5]
dataHold4<-dataHold3[,1:25]
dataHold4$CA_01<-as.factor(dataHold4$CA_01)
str(dataHold4)
dataHold4$Year<-as.factor(dataHold4$Year)
dataHold4$Month<-as.factor(dataHold4$Month)
dataHold4$Day<-as.factor(dataHold4$Day)

attach(dataDM3)

str(dataHold4)

dataDM3$CA_01<-as.factor(dataDM3$CA_01)
str(dataDM3)
dataDM3$Year<-as.factor(dataDM3$Year)
dataDM3$Month<-as.factor(dataDM3$Month)
dataDM3$Day<-as.factor(dataDM3$Day)
dataDM3$GOOD<-as.factor(dataDM3$GOOD)

require(randomForest)

rf<-randomForest(GOOD~.,data=dataDM3,mtry=round(sqrt(21)))
pred.prob<-predict(rf,newdata=dataDM3,type="prob")
pred<-ifelse(pred.prob[,2]>=0.5,1,0)
conf.matr<-as.matrix(table(pred,dataDM3$GOOD))
conf.matr

# Create n-equally sized folds
folds <- function(n,data){
  return(cut(seq(1,nrow(data)),breaks=n,labels=FALSE))
}

# CV score using k-fold cross-validation for nnet function

cvnnet<-function(n=10,size=5,data,seed1,seed2,by){
  
  m<-seq(from=0,to=1,by=by)                     # Provides threshold ranges
  len.m<-length(m)
  
  nfolds<-folds(n,data)                         # Provides indices for the n subsets                          
  
  set.seed(seed1) 
  
  data1 <- data[sample(nrow(data)),]            # Mixes up the data randomly
  
  misclass<-rep(NA,len.m)                       # Items to store results
  gen.est<-rep(NA,len.m)
  threshold<-rep(NA,len.m)
  CVstore <- rep(NA,n)
  
  
  for(j in 1:len.m){                            # For each of the thresholds
    
    thresh<-m[j]
    
    for(i in 1:n){                              # K-fold cross-validation
  
      Indexi <- which(nfolds==i,arr.ind=TRUE)
      test1 <- data1[Indexi,]
      train1 <- data1[-Indexi,]
  
      train2<- nnet(as.factor(GOOD)~.,data=train1,size=size,trace=F)
  
      test2<-ifelse(predict(train2,newdata=test1,type="raw")>=thresh,"1","0")
      CVstore[i]<-sum(test2!=as.factor(test1$GOOD))/nrow(test1)
    }
  
    set.seed(seed2)                             # Misclassification error over the entire dataset
    goodNN<- nnet(as.factor(GOOD)~.,data=data,size=size,trace=F)
    NNpred<-ifelse(predict(goodNN,newdata=data,type='raw')>=thresh,"1","0")

  
    misclass[j]<-sum(NNpred!=as.factor(data$GOOD))/nrow(data)
    gen.est[j]<-mean(CVstore)
    
    threshold[j]<-thresh
    
  }
  
  return(data.frame(threshold,misclass,gen.est))# Return threshold, training error, test error estimate

}

cvneuralnet<-function(n=10,size=5,data,seed1,seed2,by,lin.form){
  
  m<-seq(from=0,to=1,by=by)                     # Provides threshold ranges
  len.m<-length(m)
  
  nfolds<-folds(n,data)                         # Provides indices for the n subsets                          
  
  set.seed(seed1) 
  
  data1 <- data[sample(nrow(data)),]            # Mixes up the data randomly
  
  misclass<-rep(NA,len.m)                       # Items to store results
  gen.est<-rep(NA,len.m)
  threshold<-rep(NA,len.m)
  CVstore <- rep(NA,n)
  
  
  for(j in 1:len.m){                            # For each of the thresholds
    
    thresh<-m[j]
    
    for(i in 1:n){                              # K-fold cross-validation
      
      Indexi <- which(nfolds==i,arr.ind=TRUE)
      test1 <- data1[Indexi,]
      train1 <- data1[-Indexi,]
      
      train2<- neuralnet(GOOD~lin.form,data=train1,hidden=size)
      
      test2<-ifelse(predict(train2,newdata=test1,type="raw")>=thresh,"1","0")
      CVstore[i]<-sum(test2!=as.factor(test1$GOOD))/nrow(test1)
    }
    
    set.seed(seed2)                             # Misclassification error over the entire dataset
    goodNN<- neuralnet(GOOD~lin.form,data=data,hidden=size)
    NNpred<-ifelse(predict(goodNN,newdata=data,type='raw')>=thresh,"1","0")
    
    
    misclass[j]<-sum(NNpred!=as.factor(data$GOOD))/nrow(data)
    gen.est[j]<-mean(CVstore)
    
    threshold[j]<-thresh
    
  }
  
  return(data.frame(threshold,misclass,gen.est))# Return threshold, training error, test error estimate
  
}

result1<-cvnnet(5,3,data,seed1=7855,seed2=1234,by=0.05)
result2<-cvnnet(5,4,data,seed1=7245,seed2=5132,by=0.05)
result3<-cvnnet(5,5,data,seed1=2135,seed2=8623,by=0.05)
result4<-cvnnet(5,6,data,seed1=9234,seed2=5423,by=0.05)
result5<-cvnnet(5,7,data,seed1=0214,seed2=8721,by=0.05)
result6<-cvnnet(5,8,data,seed1=1238,seed2=6312,by=0.05)

Holdback<-read.csv("fill.hold.csv",header=T)
detach(data)
attach(Holdback)
NNetModel1<- nnet(as.factor(GOOD)~.,data=data1,size=3,trace=F)
plotnet(NNetModel1)
NNPred1<-ifelse(predict(NNetModel1,newdata=data1))
                
dataDM4<-cbind(dataDM3[,seq(5,26)],dataDM3[,seq(1,4)])
str(dataDM4)
                
cv_function<-function(k=10,seed=6,data,threshold,model=1,np){
                  
  n<-dim(data)[1]
  size<-n%/%k
  set.seed(seed)
  a<-runif(n)
  b<-rank(a)
  block<-(b-1)%/%size+1
  block<-as.factor(block)
                  
  result<-matrix(NA, nrow=k, ncol=1)
  for(i in 1:k){
    dat.train<-data[!(block==i),]
    dat.test<-data[block==i,]
    total.n<-dim(dat.test)[1]
    if(model==1){
      rf<-randomForest(GOOD~.,data=dat.train,mtry=round(sqrt(np)))
      pred.prob<-predict(rf,newdata=dat.test,type="prob")
      pred<-ifelse(pred.prob[,2]>=threshold,1,0)
      conf.matr<-as.matrix(table(pred,dat.test$GOOD))
      }
                    if(model==2){
                      bag<-randomForest(GOOD~.,data=dat.train,mtry=np)
                      pred.prob<-predict(bag,newdata=dat.test,type="prob")
                      pred<-ifelse(pred.prob[,2]>=threshold,1,0)
                      conf.matr<-as.matrix(table(pred,dat.test$GOOD))
                    }
                    if(model==3){
                      boost<-gbm(GOOD~.,data=dat.train,distribution="bernoulli",n.trees=5000)
                      pred.prob<-predict(boost,newdata=dat.test,type="response",n.trees=5000)
                      pred<-ifelse(pred.prob>=threshold,1,0)
                      conf.matr<-as.matrix(table(pred,dat.test$GOOD))
                    }
                    result[i,1]<-sum(diag(conf.matr))/total.n
                    #result[i,2]<-conf.matr[2,2]/sum(conf.matr[,2])
                    #result[i,3]<-conf.matr[1,1]/sum(conf.matr[,1])
                  }
                  return(accuracy=result[,1])
                }
                          
attach(dataDM4)
numb1<-cv_function(k=5,seed=1234,data=dataDM4,threshold=0.3831068,model=2,np=25)
1-mean(numb1)
install.packages("pROC")
library(pROC)
#input arguments class and pred must be numeric
roc_func<-function(class,pred,plot=TRUE){
  roc_obj <- roc(class,pred)
  roc_auc<-auc(roc_obj)
  ci_auc<-ci.auc(roc_obj)
  ci.thre<-ci(roc_obj, of="thresholds", thresholds="best")
  
  if(plot){
    plot.roc(roc_obj)
    ciobj <- ci.se(roc_obj,specificities=seq(0,1,0.05))
    plot(ciobj, type="shape", col="#1c61b6AA")
    plot(ci.thre) 
  }
  
  return(list(AUC=roc_auc, CI_of_AUC=ci_auc, Thresholds=ci.thre))
}

rf<-randomForest(GOOD~.,data=dataDM4,mtry=round(sqrt(25)),importance=TRUE)
pred2<-predict(rf,type="prob")
roc_func(class=(as.numeric(GOOD)-1),pred=pred2[,2])

pred<-ifelse(pred2[,2]>=0.6504054,1,0)
conf.matr<-as.matrix(table(pred,dataDM3$GOOD))
conf.matr

sum(pred!=as.factor(dataDM4$GOOD))/nrow(dataDM4)

rf<-randomForest(GOOD~.,data=data,mtry=round(sqrt(21)),importance=TRUE)
pred2<-predict(rf,type="prob")
roc_func(class=(as.numeric(GOOD)-1),pred=pred2[,2])
pred<-ifelse(pred2[,2]>=0.6246951,1,0)
sum(pred!=as.factor(data$GOOD))/nrow(data)

library(randomForest)
set.seed(6)
bag.fit<-randomForest(GOOD~.,data=dataDM4,mtry=25,importance=TRUE)
pred1<-predict(bag.fit, type="prob")
roc_func(class=(as.numeric(GOOD)-1),pred=pred1[,2])
pred<-ifelse(pred1[,2]>=0.3010231,1,0)
sum(pred!=as.factor(data$GOOD))/nrow(data)


##########################################################3
str(dataDM4)
set.seed(6);rf<-randomForest(GOOD~.,data=dataDM4,mtry=round(sqrt(25)),importance=TRUE)
pred2<-predict(rf,type="prob")
pred<-ifelse(pred2[,2]>=0.6504054,1,0)
sum(pred!=as.factor(dataDM4$GOOD))/nrow(dataDM4)

predFINAL<-predict(rf,newdata=dataHold4,type="prob")
GOOD<-ifelse(predFINAL[,2]>=0.6504054,1,0)
BAD<-ifelse(predFINAL[,2]<0.6504054,1,0)
str(predFINAL2)
prediction<-cbind(dataHold$app_id,GOOD,BAD)

app_id<-prediction[,1]
prediction2<-cbind(app_id,GOOD,BAD)
str(prediction2)

write.csv(prediction2,"predictions-Rampla-Juniors.csv",row.names=FALSE)

Finished<-read.csv("predictions-Rampla-Juniors.csv",header=T)
identical(dataHold$app_id,Finished$app_id)
