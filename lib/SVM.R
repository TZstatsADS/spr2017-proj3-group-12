#################
### SVM model ###
#################
### Author: Boxuan Zhao
### Project 3
### ADS spring 2017


#load porper packages
list.of.packages <- c("e1071", "ggplot2")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(e1071)
#library(ggplot2)
source("./lib/train.R")
source("./lib/test.R")
source("./lib/cross_validation.R")

dat.ori.train = read.csv("./data/sift_features_train.csv",header = T)

dat.ori.train = t(dat.ori.train)


dat.ori.test = read.csv("./data/sift_features_test.csv",header = T)

dat.ori.test = t(dat.ori.test)


dat.simp.train = read.csv("./data/sift_features_new_train.csv",header = T)

dat.simp.train = t(dat.simp.train)


dat.simp.test = read.csv("./data/sift_features_new_test.csv",header = T)

dat.simp.test = t(dat.simp.test)


class.train = read.csv("./data/labels_train.csv",header = T)

class.train = class.train[,1]


class.test = read.csv("./data/labels_test.csv",header = T)

class.test = class.test[,1]

####################
#Do not Use feature#
####################

#data.all = data.all[which(apply(data.all,1,sd)>summary(apply(data.all,1,sd))[2]),]

############################################
#SVM with soft margin on Simplified Feature#
############################################

###################
#cost = 0.01

#cv.error = 0.2993333

#   cost  cv.error
#1 1e-04 0.7993333
#2 1e-03 0.3040000
#3 1e-02 0.2993333
#4 1e-01 0.2993333

#Test Error
#  0.228
#################

#$cost
#[1] 0.003162278    10^-2.5

#$cv.error
#[1] 0.2973333

#$frame
#cost  cv.error
#1 0.031622777 0.2993333
#2 0.003162278 0.2973333

#Test error
#0.222

#################
#FINAL VALUE: 10^-2.5 Test err = 0.222  val.err 0.297333

margin.cv.simp.1 = system.time({

  SVM.Margin.par = svm.margin.cv(dat.train = dat.simp.train, class.train = class.train, cost = 10^c(-2.5))

  })

margin.train.simp.1 = system.time({
  
SVM.final.margin.model = Train.SVM.margin(X = dat.simp.train,Y = class.train,cost = SVM.Margin.par$cost)

})

margin.pred.simp.1 = system.time({
  
SVM.margin.test.error = Test.SVM(SVM.final.margin.model,val = dat.simp.test,class = class.test)

})

SVM.Margin.par

SVM.margin.test.error

#############################################################################################
###############################################
#SVM with soft margin on Original Sift Feature#
###############################################

#$cost
#[1] 1e-04

#$cv.error
#[1] 0.8913333

#$frame
#cost  cv.error
#1 1e-04 0.8913333
#2 1e-03 0.8913333
#3 1e-02 0.8913333
#4 1e-01 0.8913333
#> SVM.margin.test.error
#[1] 0.51


margin.cv.ori.2 = system.time({
  
  SVM.Margin.par = svm.margin.cv(dat.train = dat.ori.train, class.train = class.train, cost = 10^(-2.5))
  
})

margin.train.ori.2 = system.time({
  
  SVM.final.margin.mdoel = Train.SVM.margin(X = dat.ori.train,Y = class.train,cost = SVM.Margin.par$cost)
  
})

margin.pred.ori.2 = system.time({
  
  SVM.margin.test.error = Test.SVM(SVM.final.margin.mdoel,val = dat.ori.test,class = class.test)
  
})

SVM.Margin.par

SVM.margin.test.error

############################################################
#SVM with soft margin and kernel on Simplified sift Feature#
############################################################

kernel.cv.simp = system.time({
  
SVM.kernel.par = svm.kernel.cv(dat.train = dat.simp.train, class.train = class.train, cost = 10^( -4: -1), gamma = 2^(seq(-10, 0, 0.5)))

})

kernel.train.simp = system.time({
  
SVM.kernel.final.model = Train.SVM.kernel(X = dat.simp.train,Y = class.train,cost = SVM.kernel.par$cost,gamma = SVM.kernel.par$gamma)

})

kernel.pred.simp = system.time({
  
SVM.kernel.test.error = Test.SVM(SVM.kernel.final.model,val = dat.simp.test,class = class.test)

})

SVM.kernel.par

SVM.kernel.test.error

####################################################################################################################################
##########################################################
#SVM with soft margin and kernel on Original sift Feature#
##########################################################

kernel.cv.ori = system.time({
  
  SVM.kernel.par = svm.kernel.cv(dat.train = dat.ori.train, class.train = class.train, cost = 10^( -4: -1), gamma = 2^(seq(-10, 0, 0.5)))
  
})

kernel.train.ori = system.time({
  
  SVM.kernel.final.model = Train.SVM.kernel(X = dat.ori.train,Y = class.train,cost = SVM.kernel.par$cost,gamma = SVM.kernel.par$gamma)
  
})

kernel.pred.ori = system.time({
  
  SVM.kernel.test.error = Test.SVM(SVM.kernel.final.model,val = dat.ori.test,class = class.test)
  
})

SVM.kernel.par

SVM.kernel.test.error
