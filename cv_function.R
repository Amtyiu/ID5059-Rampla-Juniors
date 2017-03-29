#k: k fold model selection
#n: sample 
#data: original data in data frame format with the first column being good
#model: 1 ramdom forest 2:bagging 3:boosting all from James
#       for model 1&2, GOOD must be converted to factor variable first
#       for model 3, GOOD must be numerical 0 or 1
cv_function<-function(k=10,seed=6,data,threshold,model=1){
  
  n<-dim(data)[1]
  size<-n%/%k
  set.seed(seed)
  a<-runif(n)
  b<-rank(a)
  block<-(b-1)%/%size+1
  block<-as.factor(block)
  
  result<-matrix(NA, nrow=k, ncol=3)
  for(i in 1:k){
    dat.train<-data[!(block==i),]
    dat.test<-data[block==i,]
    total.n<-dim(dat.test)[1]
    if(model==1){
      rf<-randomForest(GOOD~.,data=dat.train,mtry=round(sqrt(21)))
      pred.prob<-predict(rf,newdata=dat.test,type="prob")
      pred<-ifelse(pred.prob[,2]>=threshold,1,0)
      conf.matr<-as.matrix(table(pred,dat.test$GOOD))
    }
    if(model==2){
      bag<-randomForest(GOOD~.,data=dat.train,mtry=21)
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
    result[i,2]<-conf.matr[2,2]/sum(conf.matr[,2])
    result[i,3]<-conf.matr[1,1]/sum(conf.matr[,1])
  }
  return(data.frame(accuracy=result[,1],sensitivity=result[,2],specificity=result[,3]))
}


