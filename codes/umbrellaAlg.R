### This function is the NP-umbrella algorithm, corresponding to Fig 3 in the main text. With labeled training data and testing data as input, the function returns the type I and type II errors for different NP-classifiers.
### Practitioners can apply this function to do classificaiton directly.

library(nproc)
library(dsda)

umbrellaAlg<-function(X_train,y_train,X_testn,y_test,d,a)
{
  fit  = npc(X_train, y_train,split = 7, method = "penlog", delta=d, alpha=a, split.ratio = "adaptive")
  fitnb  = npc(X_train, y_train,  split = 7, method = "nb", delta=d,alpha=a, split.ratio = "adaptive")
  fitsvm = npc(X_train, y_train, split = 7,  method = "svm", delta=d, alpha=a, split.ratio = "adaptive")
  fitrf = npc(X_train, y_train, split = 7, method = "randomforest", delta=d, alpha=a, split.ratio = "adaptive")
  fitslda = npc(X_train, y_train, split = 7, method = "slda", delta=d, alpha=a, split.ratio = "adaptive")  
  
  svm_model = svm(X_train,y_train,type="C-classification")
  svm_pred  = predict(svm_model,as.matrix(X_testn))
  
  nb_model  = naiveBayes(X_train, as.factor(y_train))
  nb_pred   = predict(nb_model,as.data.frame(X_testn),type="class")
  
  penlog_model = cv.glmnet(X_train, y_train, family="binomial")
  penlog_pred  =predict(penlog_model, as.matrix(X_testn),type="class")
  rf_model  = randomForest(as.matrix(X_train),as.factor(y_train))
  rf_pred   = predict(rf_model,as.matrix(X_testn),type="response")
  dsda_model = dsda(as.matrix(X_train),y_train)
  dsda_pred  = predict.dsda(dsda_model,as.matrix(X_testn))
  
  pred      = predict(fit,as.matrix(X_testn))
  prednb    = predict(fitnb,as.matrix(X_testn))
  predsvm   = predict(fitsvm,as.matrix(X_testn))
  predrf    = predict(fitrf,as.matrix(X_testn))
  predslda  = predict(fitslda,as.matrix(X_testn))
  
  I      = mean(pred$pred.label[y_test==0]==1)
  II     = mean(pred$pred.label[y_test==1]==0)
  Inb   = mean(prednb$pred.label[y_test==0]==1)
  IInb   = mean(prednb$pred.label[y_test==1]==0)
  Isvm   = mean(predsvm$pred.label[y_test==0]==1)
  IIsvm  = mean(predsvm$pred.label[y_test==1]==0)
  Irf  = mean(predrf$pred.label[y_test==0]==1)
  IIrf  = mean(predrf$pred.label[y_test==1]==0)
  Idsda  = mean(predslda$pred.label[y_test==0]==1)
  IIdsda = mean(predslda$pred.label[y_test==1]==0)
  
  
  Inbr   = mean(nb_pred[y_test==0]==1)
  IInbr  = mean(nb_pred[y_test==1]==0)
  Isvmr  = mean(svm_pred[y_test==0]==1)
  IIsvmr = mean(svm_pred[y_test==1]==0)
  Ir     = mean(penlog_pred[y_test==0]==1)
  IIr    = mean(penlog_pred[y_test==1]==0)
  Irfr     = mean(rf_pred[y_test==0]==1)
  IIrfr   = mean(rf_pred[y_test==1]==0)
  Idsdar   = mean(dsda_pred[y_test==0]==1)
  IIdsdar  = mean(dsda_pred[y_test==1]==0)
  
  result <-list(I=I,II=II, Inb=Inb,IInb=IInb,Isvm=Isvm,IIsvm=IIsvm,Irf=Irf,IIrf=IIrf, Idsda=Idsda, IIdsda=IIdsda, Inbr=Inbr,IInbr=IInbr,Isvmr=Isvmr,IIsvmr=IIsvmr,Ir=Ir,IIr=IIr,Irfr=Irfr,IIrfr=IIrfr,Idsdar=Idsdar,IIdsdar=IIdsdar)
}