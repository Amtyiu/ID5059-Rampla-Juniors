#-----------------------------------------------naiveBAyes------------------------------------------------#
library(e1071)
library(randomForest)

#---------------------------------------------------------------------------------------------------------#
fittedModel <- naiveBayes(GOOD ~ ., data = imp.dm)

# get class predictions
predictedClass<- predict(fittedModel, newdata=imp.dm)

# confusion matrix
table(predictedClass, imp.dm$GOOD)

