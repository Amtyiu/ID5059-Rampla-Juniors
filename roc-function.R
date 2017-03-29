#Area under the curve
library(pROC)
#input arguments class and pred must be numeric
roc_func<-function(class,pred,plot=TRUE){
  roc_obj <- roc(class,pred)
  roc_auc<-auc(roc_obj)
  ci_auc<-ci.auc(roc_obj)
  ci.thre<-ci(roc_obj, of="thresholds", thresholds="best")
  
  if(plot){
    plot.roc(roc_obj)
    ciobj <- ci.se(roc_obj,specificities=seq(0,1,0.05))
    plot(ciobj, type="shape", col="#1c61b6AA")
    plot(ci.thre) 
  }
  
  return(list(AUC=roc_auc, CI_of_AUC=ci_auc, Thresholds=ci.thre))
}