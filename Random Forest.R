#-----------------------------------------------------------------------------------------------------------------#
#Random forest with tuning
#-----------------------------------------------------------------------------------------------------------------#

library(randomForest)
library(mlbench)
library(caret)

control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"
set.seed(seed)
mtry <- sqrt(21)
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(GOOD~., data=imp.dm, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_default)

#1.tune using caret lib, tuning mtry
# Random Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(seed)
mtry <- sqrt(21)
rf_random <- train(GOOD~., data=imp.dm, method="rf", metric=metric, tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random)
#best accuracy when mtry=22
#accuracy=0.8749928  kappa=0.7232773

#grid search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(seed)
tunegrid <- expand.grid(.mtry=c(5:28))
rf_gridsearch <- train(GOOD~., data=imp.dm, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)
#mtry = 26
#accuracy=0.8762552  kappa=0.7261006

#2.Tune Using Algorithm Tools, tuning mtry
x <- as.matrix(imp.dm[,-1])
y <- as.matrix(imp.dm[,1])

set.seed(seed)
bestmtry <- tuneRF(x, y, stepFactor=1.5, improve=1e-5, ntree=500)
print(bestmtry)

#3. Craft Your Own Parameter Search
#Tune Manually
#evaluate different values for ntree while holding mtry constant.

control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
tunegrid <- expand.grid(.mtry=26)
modellist <- list()
for (ntree in c(1000, 1500, 2000, 2500)) {
        set.seed(seed)
        fit <- train(GOOD~., data=imp.dm, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control, ntree=ntree)
        key <- toString(ntree)
        modellist[[key]] <- fit
}
# compare results
results <- resamples(modellist)
summary(results)
dotplot(results)
#Accuracy 
#Min. 1st Qu. Median   Mean 3rd Qu.   Max. NA's
#1000 0.8436  0.8664 0.8770 0.8751  0.8847 0.8969    0
#1500 0.8471  0.8624 0.8795 0.8755  0.8861 0.8969    0
#2000 0.8468  0.8646 0.8795 0.8755  0.8860 0.8969    0
#2500 0.8468  0.8651 0.8804 0.8756  0.8847 0.8985    0

#Kappa 
#      Min. 1st Qu. Median   Mean 3rd Qu.   Max. NA's
#1000 0.6562  0.7019 0.7287 0.7238  0.7468 0.7739    0
#1500 0.6645  0.6973 0.7329 0.7245  0.7483 0.7734    0
#2000 0.6581  0.6985 0.7332 0.7246  0.7481 0.7734    0
#2500 0.6581  0.6995 0.7343 0.7249  0.7466 0.7774    0


#f a custom random forest algorithm for use with caret that takes both an mtry and ntree parameters

customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
        randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
        predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
        predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes


# train model
control <- trainControl(method="repeatedcv", number=10, repeats=3)
tunegrid <- expand.grid(.mtry=c(1:15), .ntree=c(1000, 1500, 2000, 2500))
set.seed(seed)
custom <- train(GOOD~., data=imp.dm, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)
summary(custom)
plot(custom)

#train model
control <- trainControl(method="repeatedcv", number=10, repeats=3)
tunegrid <- expand.grid(.mtry=c(1:15), .ntree=c(1000, 1500, 2000, 2500))
set.seed(seed)
custom <- train(GOOD~., data=imp.dm, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)
summary(custom)
plot(custom)