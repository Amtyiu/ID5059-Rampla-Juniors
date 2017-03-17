
pcr <- princomp(~ disp_income + cust_age + time_emp + I_01 + I_02 + I_03 + I_04 + I_05 + I_06 + D_01 + D_02 + ER_01 + ER_02 + P_01 + S_01 + CA_03 + CA_02 + S_02, cor = FALSE, scores = TRUE)
summary(pcr, loadings = T)
head(pcr$scores)

scores1 <- pcr$scores[,1]
scores2 <- pcr$scores[,2]
scores3 <- pcr$scores[,3]
scores4 <- pcr$scores[,4]

par(mfrow=c(1,1))
plot(scores1,scores2,ylim=range(scores1),xlab="PC1",ylab="PC2",type="n",lwd=2)
text(scores1,scores2,labels=abbreviate(imp.dm[,1]), cex=0.7,lwd=2)

plot(scores1,scores2,ylim=range(scores1),xlab="PC1",ylab="PC2",type="n",lwd=2)
text(scores1,scores2,labels=abbreviate(imp.dm[,3]), cex=0.7,lwd=2)

plot(scores1,scores2,ylim=range(scores1),xlab="PC1",ylab="PC2",type="n",lwd=2)
text(scores1,scores2,labels=abbreviate(imp.dm[,6]), cex=0.7,lwd=2)

plot(scores1,scores2,ylim=range(scores1),xlab="PC1",ylab="PC2",type="n",lwd=2)
text(scores1,scores2,labels=abbreviate(imp.dm[,21]), cex=0.7,lwd=2)


install.packages("scatterplot3d")
library(scatterplot3d)

scatterplot3d(x=scores1, y=scores2, z=scores3)