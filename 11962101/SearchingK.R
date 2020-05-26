# The code produces the topics and keywords for each topic
# Note that package dsda was installed directed from the tar.gz file downloaded online. All the codes are run under R version 3.6.1

set.seed(2019)
library(nproc)
library(topicmodels)
library("e1071")
library(dsda)
library(glmnet)
library(randomForest)

#load('Input/Alldata.RData')
#load('Input/count.RData')

load('Alldata.RData')
load('count.RData')

# # Get the dimensions
# n_1  = dim(A)[1]
# n_2  = dim(B)[1]
# 
# 
# p   = dim(A)[2]
# 
# # Generate labels
# y_1 = rep(0,n_1)
# y_2 = rep(1,n_2)



repl = 50
k = 10
#k=5



#We have k topics in total, for each topic, each iteration, we keep the top #noword many words.
noword = 20
wordseq1=matrix(0,noword,repl)
wordseq2=matrix(0,noword,repl)
wordseq3=matrix(0,noword,repl)
wordseq4=matrix(0,noword,repl)
wordseq5=matrix(0,noword,repl)
wordseq6=matrix(0,noword,repl)
wordseq7=matrix(0,noword,repl)
wordseq8=matrix(0,noword,repl)
wordseq9=matrix(0,noword,repl)
wordseq10=matrix(0,noword,repl)


for (i in 1:repl){

  print(i)
  data_shuffle = Alldata[sample(nrow(Alldata)),]
 
  r = 2
  data1     = data_shuffle[1:floor(nrow(Alldata)/r),]

  
  # Finding key words for each topic
  object = LDA(data1, k, method = "Gibbs", control= NULL, model = NULL)
  wordprob = object@beta
  topic1   = sort(wordprob[1,], decreasing=TRUE, index.return=TRUE)$ix
  topic2   = sort(wordprob[2,], decreasing=TRUE, index.return=TRUE)$ix
  topic3   = sort(wordprob[3,], decreasing=TRUE, index.return=TRUE)$ix
  topic4   = sort(wordprob[4,], decreasing=TRUE, index.return=TRUE)$ix
  topic5   = sort(wordprob[5,], decreasing=TRUE, index.return=TRUE)$ix
  topic6   = sort(wordprob[6,], decreasing=TRUE, index.return=TRUE)$ix
  topic7   = sort(wordprob[7,], decreasing=TRUE, index.return=TRUE)$ix
  topic8   = sort(wordprob[8,], decreasing=TRUE, index.return=TRUE)$ix
  topic9   = sort(wordprob[9,], decreasing=TRUE, index.return=TRUE)$ix
  topic10   = sort(wordprob[10,], decreasing=TRUE, index.return=TRUE)$ix
 
  
  
  wordseq1[,i] = as.vector(count[topic1[1:noword],])
  wordseq2[,i] = as.vector(count[topic2[1:noword],])
  wordseq3[,i] = as.vector(count[topic3[1:noword],])
  wordseq4[,i] = as.vector(count[topic4[1:noword],])
  wordseq5[,i] = as.vector(count[topic5[1:noword],])
  wordseq6[,i] = as.vector(count[topic6[1:noword],])
  wordseq7[,i] = as.vector(count[topic7[1:noword],])
  wordseq8[,i] = as.vector(count[topic8[1:noword],])
  wordseq9[,i] = as.vector(count[topic9[1:noword],])
  wordseq10[,i] = as.vector(count[topic10[1:noword],])
 
}

save(wordseq1,wordseq2,wordseq3,wordseq4,wordseq5,wordseq6,wordseq7,wordseq8,wordseq9,wordseq10,file="wordk10.RData")


for (i in 1:50){
  print(i)
  print(cat("topic 1", wordseq1[,i]))
  print(cat("topic 2", wordseq2[,i]))
  print(cat("topic 3",wordseq3[,i]))
  print(cat("topic 4",wordseq4[,i]))
  print(cat("topic 5",wordseq5[,i]))
  print(cat("topic 6", wordseq6[,i]))
  print(cat("topic 7", wordseq7[,i]))
  print(cat("topic 8",wordseq8[,i]))
  print(cat("topic 9",wordseq9[,i]))
  print(cat("topic 10",wordseq10[,i]))
}




