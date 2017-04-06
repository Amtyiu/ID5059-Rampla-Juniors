# ID 5059 

# Neural Network

library(nnet)
library(fields)
library(rgl)
library(caret)

library(neuralnet)
data<-read.csv("dat_withoutNA.csv",header=T)
attach(data)



# Create n-equally sized folds
folds <- function(n){
  return(cut(seq(1,nrow(data)),breaks=n,labels=FALSE))
}


cvnnet<-function(n=10,size=5,data,seed1,seed2){
  
  nfolds<-folds(n)
  
  set.seed(seed1) 
  
  data1 <- data[sample(nrow(data)),]

  CVstore <- rep(NA,n)

  for(i in 1:n){
  
    Indexi <- which(nfolds==i,arr.ind=TRUE)
    test1 <- data1[Indexi,]
    train1 <- data1[-Indexi,]
  
    train2<- nnet(as.factor(GOOD)~.,data=train1,size=size,trace=F)
  
    test2<-predict(train2,newdata=test1,type="class")
    CVstore[i]<-sum(test2!=as.factor(test1$GOOD))/nrow(test1)
  }
  
  set.seed(seed2)
  goodNN<- nnet(as.factor(GOOD)~.,data=data,size=size,trace=F)
  NNpred<-predict(goodNN,newdata=data,type='class')

  
  misclass<-sum(NNpred!=as.factor(data$GOOD))/nrow(data)
  
  return(c(misclass,mean(CVstore)))
}
cvnnet(10,3,data,seed1=7855,seed2=1234)
cvnnet(10,4,data,seed1=7245,seed2=5132)
cvnnet(10,5,data,seed1=2135,seed2=8623)
cvnnet(10,6,data,seed1=9234,seed2=5423)
cvnnet(10,7,data,seed1=0214,seed2=8721)
cvnnet(10,8,data,seed1=1238,seed2=6312)
