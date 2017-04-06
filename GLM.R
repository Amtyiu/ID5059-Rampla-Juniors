
#---------------------------------Generalised Linear Model -------------------------------------------------------------#

library(car)
library(MuMIn)

# Generalised Linear Model with binomial error structure. 
options(na.action = "na.fail") 
fittedglm <- glm(GOOD ~ ., data = imp.dm, family='binomial')

# predicted probabilities
preds<- predict(fittedglm, newdata=imp.dm, type='response')

Anova(fittedglm)

mystep <- step(fittedglm, direction = "both")
summary(mystep)
preds.step <- predict(mystep, newdata=imp.dm, type='response')

vif(mystep)
# convert to a class, based on a threshold
#predictClass<- ifelse(preds>mean(imp.dm$GOOD), 1, 0)
#predictClass<- ifelse(preds>imp.dm$GOOD, 1, 0)

predictClass<- ifelse(preds>0.6366192, 1, 0)

# confusion matrix
table(predictClass, imp.dm$GOOD)

predictClassstep<- ifelse(preds.step>0.6366192, 1, 0)
table(predictClassstep, imp.dm$GOOD)
sum(diag(as.matrix(table(predictClassstep, imp.dm$GOOD))))/nrow(imp.dm)

#GLM with interactions
fittedglm.int <- glm(GOOD ~ . + CA_01*D_01 + CA_01*D_02 + CA_01*CA_02 + CA_01*disp_income + res_indicator*I_01 + res_indicator*I_02 + res_indicator*I_03 + res_indicator*I_04 + res_indicator*I_05, data = imp.dm, family='binomial')
summary(fittedglm.int)

step.int <- step(fittedglm.int, direction = "both")
summary(step.int)

preds.step.int <- predict(step.int, newdata=imp.dm, type='response')
predictClassstepint <- ifelse(preds.step.int>0.6366192, 1, 0)
table(predictClassstepint, imp.dm$GOOD)

sum(diag(as.matrix(table(predictClassstepint, imp.dm$GOOD))))/nrow(imp.dm)
