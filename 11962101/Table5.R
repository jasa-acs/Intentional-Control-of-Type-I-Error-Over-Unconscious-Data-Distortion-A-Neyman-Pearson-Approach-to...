### For Table 5, we will apply NP-sLDA trained on GDQ1 (first quarter data in GuangDong province),
### to other datasets, including GDQ2, GDQ3, GDQ4 and NGD.

load('data/Input/dataGDQ1.RData')
A1 = A
B1 = B
load('data/Input/dataGDQ2.RData')
A2 = A
B2 = B
load('data/Input/dataGDQ3.RData')
A3 = A
B3 = B
load('data/Input/dataGDQ4.RData')
A4 = A
B4 = B
load('data/Input/dataNGD.RData')
ANGD = A
BNGD = B


# start.experiment<-Sys.time()

library(nproc)
library(topicmodels)
library("e1071")
library(dsda)


# Get the dimensions
n_1  = dim(A1)[1]
n_2  = dim(B1)[1]

p   = dim(A1)[2]

# Generate labels
y_1 = rep(0,n_1)
y_2 = rep(1,n_2)




k = 10
Ipenlog  = rep(0,4)
IIpenlog = rep(0,4)

Idsda = rep(0,4)
IIdsda = rep(0,4)

set.seed(2019)
  data1     = rbind(A1,B1)

  # start.LDA <- Sys.time()
  object = LDA(data1, k, method = "Gibbs", control= NULL, model = NULL)
  # end.LDA <- Sys.time()
  # time.LDA <- end.LDA - start.LDA

  X_train = object@gamma
  y_train = c(y_1,y_2)
  
  X_testGD2    = rbind(A2,B2)
  X_testnGD2   = posterior(object,X_testGD2)$topics
  y_testGD2    = c(rep(0,dim(A2)[1]),rep(1,dim(B2)[1]))
  
  X_testGD3    = rbind(A3,B3)
  X_testnGD3   = posterior(object,X_testGD3)$topics
  y_testGD3    = c(rep(0,dim(A3)[1]),rep(1,dim(B3)[1]))
  
  X_testGD4    = rbind(A4,B4)
  X_testnGD4   = posterior(object,X_testGD4)$topics
  y_testGD4    = c(rep(0,dim(A4)[1]),rep(1,dim(B4)[1]))
  
  X_testNGD    = rbind(ANGD,BNGD)
  X_testnNGD   = posterior(object,X_testNGD)$topics
  y_testNGD    = c(rep(0,dim(ANGD)[1]),rep(1,dim(BNGD)[1]))
  
  
  # 
  # start.Umbrella <- Sys.time()
  fitslda = npc(X_train, y_train, split= 7, method = "slda", delta= 0.3,alpha = 0.1, split.ratio = "adaptive")
  
  predsldaGD2  = predict(fitslda,as.matrix(X_testnGD2))
  predsldaGD3  = predict(fitslda,as.matrix(X_testnGD3))
  predsldaGD4  = predict(fitslda,as.matrix(X_testnGD4))
  predsldaNGD  = predict(fitslda,as.matrix(X_testnNGD))
  
 
  
  Idsda[1]  = mean(predsldaGD2$pred.label[y_testGD2==0]==1)
  Idsda[2]  = mean(predsldaGD3$pred.label[y_testGD3==0]==1)
  Idsda[3]  = mean(predsldaGD4$pred.label[y_testGD4==0]==1)
  Idsda[4]  = mean(predsldaNGD$pred.label[y_testNGD==0]==1)
  
  IIdsda[1] = mean(predsldaGD2$pred.label[y_testGD2==1]==0)
  IIdsda[2] = mean(predsldaGD3$pred.label[y_testGD3==1]==0)
  IIdsda[3] = mean(predsldaGD4$pred.label[y_testGD4==1]==0)
  IIdsda[4] = mean(predsldaNGD$pred.label[y_testNGD==1]==0)
  # end.Umbrella <- Sys.time()
  # time.UmbrellaAlg<- end.Umbrella - start.Umbrella
  # 
  # end.experiment<-Sys.time()
  # time.experiment<-end.experiment-start.experiment
save(Idsda,IIdsda,file="Table5.RData")
