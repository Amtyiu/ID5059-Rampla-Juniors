dat<-read.csv("dataset_modelling.csv",na.strings=c('.',""," "))

dat$GOOD<-as.factor(dat$GOOD)
dat$BAD<-as.factor(dat$BAD)

dat$time_emp[dat$time_emp==99999]<-NA

dat$CA_01<-as.factor(dat$CA_01)

dat$S_02[dat$S_02==-1]<-NA

#impute the data using random forest
library(missForest)
set.seed(6)
impute_forest1<-missForest(dat[,-c(1,3,4)],variablewise=TRUE)
fill_data<-impute_forest1$ximp

write.csv(fill_data,"filled_data1.csv",row.names=FALSE)

#data splitting
index<-is.na(dat$GOOD)
dat_withoutNA<-fill_data[!index,]
dat_withNA<-fill_data[index,]
write.csv(dat_withoutNA,"dat_withoutNA.csv",row.names=FALSE)
write.csv(dat_withNA,"test_dat_withNA.csv",row.names=FALSE)
