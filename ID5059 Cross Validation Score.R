# Load up the data

data<-read.csv("dat_withoutNA.csv",header=T)
attach(data)

require(rattle)
require(rpart.plot)
require(RColorBrewer)
require(rpart)

# ID5059 Cross-Validation

set.seed(32487); data1 <- data[sample(nrow(data)),] #Shuffle the data (but don't touch the original)

# Create n-equally sized folds
folds <- function(n){
  return(cut(seq(1,nrow(data1)),breaks=n,labels=FALSE))
}

n<-10                 # Change this for number of folds

nfolds<-folds(n)

# n-fold cross validation

CVstore <- rep(NA,n)

for(i in 1:n){

  Indexi <- which(nfolds==i,arr.ind=TRUE)
  test1 <- data1[Indexi,]
  train1 <- data1[-Indexi,]
  
  train2<- rpart(GOOD ~ disp_income + occ_code + cust_age + time_emp + res_indicator + I_01 +
                   I_02 + I_03 + I_04 + I_05 + I_06 + D_01 + D_02 + ER_01 + ER_02 + P_01 + 
                   CA_01 + CA_02 + S_01 + S_02, method="class",data=test1)
  
  
  CVstore[i]<-sum(predict(train2, type="class")!=test1$GOOD)/nrow(test1)
}

mean(CVstore)

