#===================================================LIFT=================================================================================#
library(lift)

SVMp <- predict(svm02)  #predictions from SVM model
RFp <- predict(rf.dm)   #predictions from random forest model

liftdata <- data.frame(SVMp, RFp, GOOD)   #predictions from models and the response data
plotLift(liftdata$SVMp, liftdata$GOOD, cumulative=T)
TopDecileLift(liftdata$SVMp, liftdata$GOOD)

plotLift(liftdata$RFp, liftdata$GOOD, cumulative=T)
TopDecileLift(liftdata$RFp, liftdata$GOOD)
