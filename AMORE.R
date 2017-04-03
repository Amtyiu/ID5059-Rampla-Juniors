library(AMORE) 


dat <- read.csv("C:/Users/hp/Desktop/filled_data1.csv")

for (i in 1:22) { 
  
  dat[,i] <- as.numeric(as.vector(dat)[,i]) 
  
} 

ind<-sample(2,nrow(dat),replace=TRUE,prob=c(0.7,0.3))

traindata<- dat [ind==1,]
testdata<- dat [ind==2,]


net <- newff(n.neurons=c(21,21,1), learning.rate.global=1e-6, 
             
             momentum.global=0.5, 
             
             error.criterium="LMS", Stao=NA, hidden.layer="tansig",  
             
             output.layer="purelin", method="ADAPTgdwm") 

result <- train(net, traindata[2:22], traindata[1], error.criterium="LMS", 
                
                report=TRUE, show.step=100, n.shows=5 ) 




y <- sim(result$net, testdata[2:22]) 
y[which(y<0.5)] <- 0
y[which(y>=0.5)] <- 1
sum = 0
for(i in 1:2940){
  if(y[i]==testdata[i,1]){
    sum =sum+1
  }
}
cat("?????????", sum/3028, "n")

