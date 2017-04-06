#=================================================== SVM ======================================================================================#

library(e1071)
imp.dm.without.category <- imp.dm[,-c(3,6,21)]

svm01 <- svm(GOOD ~ ., data=imp.dm.without.category, kernel ="linear")
svm02<- svm(GOOD~., data=imp.dm.without.category, kernel ="radial", cost=1, gamma=0,5)

cont <- ftable(predict(svm01), GOOD)
sum(diag(cont))/sum(cont)

cont1 <- ftable(predict(svm02), GOOD)
sum(diag(cont1))/sum(cont1)

set.seed(1)
tune.out<- tune(svm, GOOD~., data=imp.dm, kernel="radial", ranges=list(cost=c(0.1,1,10,100,1000), gamma=c(0.5,1,2,3,4)))
summary(tune.out)
# cost=1 , gamma=0,5 best 

bestmod=tune.out$best.model
summary(bestmod)
