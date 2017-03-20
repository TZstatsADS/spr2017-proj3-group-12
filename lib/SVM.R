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
library(ggplot2)
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

#################
#Data Processing#
#################


#n_case = dim(data.all)[2]

#n_feature = dim(data.all)[1]

#Split 25% of the data as testing data

#Reshuffle the data
#index = sample(1:n_case, n_case, replace = F)
#data.all = data.all[,index]
#class.all = class.all[index,]

#Transform class 0 (chicken) to -1
#class.all = ifelse(class.all == 0, -1, 1)

#Split the data
#test.index = sample(1:n_case, n_case*0.25, replace = F)

#data.other = data.all[,-test.index]

#data.other = t(data.other)

#class.other = class.all[-test.index]

#data.test = data.all[,test.index]

#data.test = t(data.test)

#class.test = class.all[test.index]


######################
#SVM with soft margin#
######################
margin.cv.simp = system.time({

  SVM.Margin.par = svm.margin.cv(dat.train = dat.simp.train, class.train = class.train, cost = c(1,2,0.01,0.5,0.05,0.00005,0.3))

  })

margin.train.simp = system.time({
  
SVM.final.margin.model = Train.SVM.margin(X = dat.simp.train,Y = class.train,cost = SVM.Margin.par$cost)

})

margin.pred.simp = system.time({
  
SVM.margin.test.error = Test.SVM(SVM.final.margin.model,val = dat.simp.test,class = class.test)

})

SVM.Margin.par

SVM.margin.test.error

#############################################################################################
margin.cv.ori = system.time({
  
  SVM.Margin.par = svm.margin.cv(dat.train = dat.ori.train, class.train = class.train, cost = c(0.01))
  
})

margin.train.ori = system.time({
  
  SVM.final.margin.mdoel = Train.SVM.margin(X = dat.ori.train,Y = class.train,cost = SVM.Margin.par$cost)
  
})

margin.pred.ori = system.time({
  
  SVM.margin.test.error = Test.SVM(SVM.final.margin.mdoel,val = dat.ori.test,class = class.test)
  
})

SVM.Margin.par

SVM.margin.test.error

#################################
#SVM with soft margin and kernel#
#################################

kernel.cv.simp = system.time({
  
SVM.kernel.par = svm.kernel.cv(dat.train = dat.simp.train, class.train = class.train, cost = 1, gamma = 0.0005)

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

kernel.cv.ori = system.time({
  
  SVM.kernel.par = svm.kernel.cv(dat.train = dat.ori.train, class.train = class.train, cost = 1, gamma = 0.0005)
  
})

kernel.train.ori = system.time({
  
  SVM.kernel.final.model = Train.SVM.kernel(X = dat.ori.train,Y = class.train,cost = SVM.kernel.par$cost,gamma = SVM.kernel.par$gamma)
  
})

kernel.pred.ori = system.time({
  
  SVM.kernel.test.error = Test.SVM(SVM.kernel.final.model,val = dat.ori.test,class = class.test)
  
})

SVM.kernel.par

SVM.kernel.test.error
