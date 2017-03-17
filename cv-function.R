#k: k fold model selection
#n: sample 
#data: original data in data frame format
#model: 1 ramdom forest; 2 logistic regression 
n<-dim(filled_data4)[1]

cv_function<-function(k=10,seed=6,data,model=1){
  
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
      pred<-predict(rf,newdata=dat.test)
      conf.matr<-as.matrix(table(pred,dat.test$GOOD))
    }
    #if(model==2){}
    result[i,1]<-sum(diag(conf.matr))/total.n
    result[i,2]<-conf.matr[2,2]/sum(conf.matr[,2])
    result[i,3]<-conf.matr[1,1]/sum(conf.matr[,1])
  }
  return(data.frame(accuracy=result[,1],sensitivity=result[,2],specificity=result[,3]))
}
 
