# Notice that package dsda was installed directed from the tar.gz file downloaded online. All the codes are run under R version 3.3.3
# This is a counterpart of Table 4, with experiments conducted using all posts from the Guangdong province instead of only GD-Q1.
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
source('code/umbrellaAlg.R')


library(nproc)
library(topicmodels)
library("e1071")
library(dsda)
library(glmnet)
library(randomForest)

A = rbind(A1,A2,A3,A4)
B = rbind(B1,B2,B3,B4)


# Get the dimensions
n_1  = dim(A)[1]
n_2  = dim(B)[1]

p   = dim(A)[2]

# Generate labels
y_1 = rep(0,n_1)
y_2 = rep(1,n_2)



repl = 100
k = 10
I  = rep(0,repl)
II = rep(0,repl)
Inb  = rep(0,repl)
IInb = rep(0,repl)
Isvm  = rep(0,repl)
IIsvm = rep(0,repl)
Irf  = rep(0,repl)
IIrf = rep(0,repl)
Idsda = rep(0,repl)
IIdsda = rep(0,repl)

Ir  = rep(0,repl)
IIr = rep(0,repl)
Inbr  = rep(0,repl)
IInbr = rep(0,repl)
Isvmr  = rep(0,repl)
IIsvmr = rep(0,repl)
Irfr  = rep(0,repl)
IIrfr = rep(0,repl)
Idsdar  = rep(0,repl)
IIdsdar = rep(0,repl)
 # ## time recorded in s for LDA
 # time.LDA = rep(0,repl)
 # 
 # ## time recorded in min for Umbrella Alg
 # time.UmbrellaAlg = rep(0,repl)
 # 
 # ## time recorded in min for experiment
 # time.experiment = rep(0,repl)
set.seed(2019)

for (i in 1:repl){
  # start.experiment<-Sys.time()
  print(i)
  A_shuffle = A[sample(nrow(A)),]
  B_shuffle = B[sample(nrow(B)),]
  
  r = 2
  
  A_train   = A_shuffle[1:floor(n_1/r),]
  B_train   = B_shuffle[1:floor(n_2/r),]
  data1     = rbind(A_train,B_train)
  
  # start.LDA <- Sys.time()
  object = LDA(data1, k, method = "Gibbs", control= NULL, model = NULL)
   # end.LDA <- Sys.time()
   # time.LDA[i] <- end.LDA - start.LDA
   
  X_train = object@gamma
  y_train = c(y_1[1:floor(n_1/r)],y_2[1:floor(n_2/r)])
  
  X_test    = rbind(A_shuffle[(floor(n_1/r)+1):n_1,],B_shuffle[(floor(n_2/r)+1):n_2,])
  X_testn   = posterior(object,X_test)$topics
  
  y_test    = c(y_1[(floor(n_1/r)+1):n_1],y_2[(floor(n_2/r)+1):n_2])
   # start.Umbrella <- Sys.time()
  result <- umbrellaAlg(X_train,y_train,X_testn,y_test,0.3,0.1)
   # end.Umbrella <- Sys.time()
   # time.UmbrellaAlg[i] <- end.Umbrella - start.Umbrella
  
 
   I[i]      = result$I
   II[i]     = result$II
   Inb[i]    = result$Inb
   IInb[i]   = result$IInb
   Isvm[i]   = result$Isvm
   IIsvm[i]  = result$IIsvm
   Irf[i]   = result$Irf
   IIrf[i]  = result$IIrf
   Idsda[i]  = result$Idsda
   IIdsda[i] = result$IIdsda
   
   
   Inbr[i]   = result$Inbr
   IInbr[i]  = result$IInbr
   Isvmr[i]  = result$Isvmr
   IIsvmr[i] = result$IIsvmr
   Ir[i]     = result$Ir
   IIr[i]    = result$IIr
   Irfr[i]     = result$Irfr
   IIrfr[i]    = result$IIrfr
   Idsdar[i]   = result$Idsdar
   IIdsdar[i]  = result$IIdsdar
print(cat("svm w.o. NPC, type I ",mean(Isvmr[1:i])))
print(cat("svm w.o. NPC, type II ",mean(IIsvmr[1:i])))
print(cat("svm w. NPC, type I ",mean(Isvm[1:i])))
print(cat("svm w. NPC, type II ",mean(IIsvm[1:i])))

print(cat("penlog w.o. NPC, type I ",mean(Ir[1:i])))
print(cat("penlog w.o. NPC, type II ",mean(IIr[1:i])))
print(cat("penlog w. NPC, type I ",mean(I[1:i])))
print(cat("penlog w. NPC, type II ",mean(II[1:i])))


print(cat("Naive Bayes w.o. NPC, type I ",mean(Inbr[1:i])))
print(cat("Naive Bayes w.o. NPC, type II ",mean(IInbr[1:i])))
print(cat("Naive Bayes w. NPC, type I ",mean(Inb[1:i])))
print(cat("Naive Bayes w. NPC, type II ",mean(IInb[1:i])))

print(cat("rf w.o. NPC, type I ", mean(Irfr[1:i])))
print(cat("rf w.o. NPC, type II ",mean(IIrfr[1:i])))
print(cat("rf w. NPC, type I ",mean(Irf[1:i])))
print(cat("rf w. NPC, type II ",mean(IIrf[1:i])))


print(cat("slda w.o. NPC, type I ", mean(Idsdar[1:i])))
print(cat("slda w.o. NPC, type II ",mean(IIdsdar[1:i])))
print(cat("slda w. NPC, type I ",mean(Idsda[1:i])))
print(cat("slda w. NPC, type II ",mean(IIdsda[1:i])))

 # end.experiment<-Sys.time()
 #  time.experiment[i]<-end.experiment-start.experiment
}
save(Isvmr,IIsvmr,Isvm,IIsvm,Ir,IIr,I,II,Inbr,IInbr,Inb,IInb,Irfr,IIrfr,Irf,IIrf,Idsdar,IIdsdar,Idsda,IIdsda,file="Table7.RData")

