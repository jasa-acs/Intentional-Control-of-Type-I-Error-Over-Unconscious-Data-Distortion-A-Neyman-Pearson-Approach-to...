### For Table 8, we will apply NP-sLDA trained on all posts from Guangdong provinces (including Q1, Q2, Q3 and Q4),
### to NGD.

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


set.seed(2019)
# start.experiment<-Sys.time()
library(nproc)
library(topicmodels)
library("e1071")
library(dsda)

k=10
data1     = rbind(A1,A2,A3,A4,B1,B2,B3,B4)

# start.LDA <- Sys.time()
object = LDA(data1, k, method = "Gibbs", control= NULL, model = NULL)
# end.LDA <- Sys.time()
# time.LDA <- end.LDA - start.LDA


X_train = object@gamma
y_train = c(rep(0,dim(rbind(A1,A2,A3,A4))[1]),rep(1,dim(rbind(B1,B2,B3,B4))[1]))
X_testNGD    = rbind(ANGD,BNGD)
X_testnNGD   = posterior(object,X_testNGD)$topics
y_testNGD    = c(rep(0,dim(ANGD)[1]),rep(1,dim(BNGD)[1]))

# start.Umbrella <- Sys.time()
fitslda = npc(X_train, y_train, split= 7, method = "slda", delta= 0.05,alpha = 0.1, split.ratio = "adaptive")
predsldaNGD  = predict(fitslda,as.matrix(X_testnNGD))
I  = mean(predsldaNGD$pred.label[y_testNGD==0]==1)
II = mean(predsldaNGD$pred.label[y_testNGD==1]==0)
 # end.Umbrella <- Sys.time()
 # time.UmbrellaAlg<- end.Umbrella - start.Umbrella
 # 
 # end.experiment<-Sys.time()
 # time.experiment<-end.experiment-start.experiment
save(I,II,file="Table8.RData")

