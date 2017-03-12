#########################################################
### Train a classification model with training images ###
#########################################################

################### Train starts

list.of.packages <- c("EBImage", "base", "data.table", "caret",'rstudioapi','randomForest','gbm')

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(EBImage) # not available (for R version 3.3.1)
library(base)
library(data.table) # for fread
library(caret)
library(rstudioapi)
library(gbm)
library(randomForest)


#clear environment
rm(list = ls())

#set directory(Change to your own)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("D:/Columbia University/Spring2017-Applied Data Science/Project_3_Bz2290/spr2017-proj3-group-12")
#setwd("../spr2017-proj3-group-12/data")

#create label, 0 represent for chicken and 1 for dog
#label<-rep(0,2000)
#label[1:1000]<-1


#Referencing test and train using gbm from lib folder
source("../spr2017-proj3-group-12/lib/test.R")
source("../spr2017-proj3-group-12/lib/train.R")
source("../spr2017-proj3-group-12/lib/cross_validation.R")

#Overview of train and test function

#train <- function(dat_train, label_train, par=NULL)

#cv.function <- function(X.train, y.train, d, K)

#dat_train is a data frame or data matrix containing the predictor variables and label_train is the vector of outcomes. 
#The number of rows in dat_train must be the same as the length of label_train

#Read in data set
sift.feature = read.csv("../spr2017-proj3-group-12/data/sift_features.csv",header = TRUE, stringsAsFactors = FALSE)
sift.class = read.csv("../spr2017-proj3-group-12/data/labels.csv", header  =TRUE, stringsAsFactors = FALSE)

#Split 25% as the test data
index = sample(1:ncol(sift.feature),ncol(sift.feature)*0.25,replace = FALSE)
sift.test = sift.feature[,index]
sift.train = sift.feature[,-index]
class.test = sift.class[index,]
class.train = sift.class[-index,]

#Remove Constant dimensions(Since scale is enabled in the gbm function)
sift.train = sift.train[which(apply(sift.train,1,sd)!=0),]


#Baseline model using GBM with decision stump
gbm_base = train(dat_train = t(sift.train),
                 label_train = factor(class.train),
                 par=data.frame(depth = 1),Ntrees = 100)

################### Train ends


######################################Play around the function starts######################
#test = sift.train[,1:100]
#class = class.train[1:100]

#fit_gbm <- gbm.fit(x=t(test), y=factor(class),
                  # n.trees=100,
                  # distribution="bernoulli",
                  # interaction.depth=3, 
                  # bag.fraction = 0.5,
                  # verbose=FALSE)

#gbm.perf(fit_gbm,method = "test")





#the cross validation process for pca processed sift data
#the error rate is 34.55%
k_folds<-10
k<-20
set.seed(1)
s <- sample(rep(1:10, c(rep(200, 10-1), 2000-(10-1)*200))) 
d<-1
cv.error <- rep(NA, k_folds)
for(i in 1:k_folds)
{
  load(paste('train_',as.character(i),'.RData',sep=''))
  load(paste('test_',as.character(i),'.RData',sep=''))
  train.data<-t(P_train[1:k,])
  test.data<-t(P_test[1:k,])
  train.label <- label[s != i]
  test.label <- label[s == i]
  par <- list(depth=d)
  fit <- train(train.data, train.label, par)
  pred <- test(fit, test.data)  
  cv.error[i] <- mean(pred != test.label)  
  print(paste(as.character(i/k_folds*100),'%',' completed',sep=''))
}
result<-c(mean(cv.error),sd(cv.error))


#The cross validation process for pixel data
#the error rate is 22.35%
k_folds<-10
k<-100
set.seed(1)
s <- sample(rep(1:10, c(rep(200, 10-1), 2000-(10-1)*200))) 
d<-3
cv.error <- rep(NA, k_folds)
for(i in 1:k_folds)
{
  load(paste('pixel_train_',as.character(i),'.RData',sep=''))
  load(paste('pixel_test_',as.character(i),'.RData',sep=''))
  train.data<-t(P_train[1:k,])
  test.data<-t(P_test[1:k,])
  train.label <- label[s != i]
  test.label <- label[s == i]
  par <- list(depth=d)
  fit <- train(train.data, train.label, par)
  pred <- test(fit, test.data)  
  cv.error[i] <- mean(pred != test.label)  
  print(paste(as.character(i/k_folds*100),'%',' completed',sep=''))
}
result<-c(mean(cv.error),sd(cv.error))

#The cross validation process for rgb pixel data
#the error rate is 16.7%
k_folds<-10
k<-100
set.seed(1)
s <- sample(rep(1:10, c(rep(200, 10-1), 2000-(10-1)*200))) 
d<-3
cv.error <- rep(NA, k_folds)
for(i in 1:k_folds)
{
  load(paste('rgb_train_',as.character(i),'.RData',sep=''))
  load(paste('rgb_test_',as.character(i),'.RData',sep=''))
  train.data<-t(P_train[1:k,])
  test.data<-t(P_test[1:k,])
  train.label <- label[s != i]
  test.label <- label[s == i]
  par <- list(depth=d)
  fit <- train(train.data, train.label, par)
  pred <- test(fit, test.data)  
  cv.error[i] <- mean(pred != test.label)  
  print(paste(as.character(i/k_folds*100),'%',' completed',sep=''))
}
result<-c(mean(cv.error),sd(cv.error))

######################################Play around the function ends######################
