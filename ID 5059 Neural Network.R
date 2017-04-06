# ID 5059 

# Neural Network

library(nnet)
library(fields)
library(rgl)
library(caret)

library(neuralnet)
data1<-read.csv("fill.pos.csv",header=T)
data2<-cbind(data1[,22:25],data1[,1:21])
data<-data2[-c(1,2,4)]
attach(data)



# Create n-equally sized folds
folds <- function(n){
  return(cut(seq(1,nrow(data)),breaks=n,labels=FALSE))
}


cvnnet<-function(n=10,size=5,data,seed1,seed2,by){
  
  m<-seq(from=0,to=1,by=by)
  len.m<-length(m)
  
  nfolds<-folds(n)
  
  set.seed(seed1) 
  
  data1 <- data[sample(nrow(data)),]
  
  misclass<-rep(NA,len.m)
  gen.est<-rep(NA,len.m)

  CVstore <- rep(NA,n)
  
  
  for(j in 1:len.m){
    
    thresh<-m[j]
    
    for(i in 1:n){
  
      Indexi <- which(nfolds==i,arr.ind=TRUE)
      test1 <- data1[Indexi,]
      train1 <- data1[-Indexi,]
  
      train2<- nnet(as.factor(GOOD)~.,data=train1,size=size,trace=F)
  
      test2<-ifelse(predict(train2,newdata=test1,type="raw")>=thresh,"1","0")
      CVstore[i]<-sum(test2!=as.factor(test1$GOOD))/nrow(test1)
    }
  
    set.seed(seed2)
    goodNN<- nnet(as.factor(GOOD)~.,data=data,size=size,trace=F)
    NNpred<-ifelse(predict(goodNN,newdata=data,type='raw')>=thresh,"1","0")

  
    misclass[j]<-sum(NNpred!=as.factor(data$GOOD))/nrow(data)
    gen.est[j]<-mean(CVstore)
    
  }
  
  return(data.frame(m,misclass,gen.est))

}
result1<-cvnnet(5,3,data,seed1=7855,seed2=1234,by=0.1)
result2<-cvnnet(5,4,data,seed1=7245,seed2=5132,by=0.1)
result3<-cvnnet(5,5,data,seed1=2135,seed2=8623,by=0.1)
result4<-cvnnet(5,6,data,seed1=9234,seed2=5423,by=0.1)
result5<-cvnnet(5,7,data,seed1=0214,seed2=8721,by=0.1)
result6<-cvnnet(10,8,data,seed1=1238,seed2=6312,by=0.1)
