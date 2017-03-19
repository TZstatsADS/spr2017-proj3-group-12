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

data.all= read.csv("./data/sift_features.csv",header = T)

class.all = read.csv("./data/labels.csv",header = T)

##################################
#New Feature(dimension reduction)#
##################################

source("./lib/feature.R")

new_feature = feature.new(data.all)

data.all = new_feature

source("./lib/train.R")
source("./lib/test.R")
source("./lib/cross_validation.R")



#write.csv(new_feature, file="./data/sift.feature.New.csv")

####################
#Do not Use feature#
####################

#data.all = data.all[which(apply(data.all,1,sd)>summary(apply(data.all,1,sd))[2]),]

#################
#Data Processing#
#################


n_case = dim(data.all)[2]

n_feature = dim(data.all)[1]

#Split 25% of the data as testing data

#Reshuffle the data
index = sample(1:n_case, n_case, replace = F)
data.all = data.all[,index]
class.all = class.all[index,]

#Transform class 0 (chicken) to -1
class.all = ifelse(class.all == 0, -1, 1)

#Split the data
test.index = sample(1:n_case, n_case*0.25, replace = F)

data.other = data.all[,-test.index]

data.other = t(data.other)

class.other = class.all[-test.index]

data.test = data.all[,test.index]

data.test = t(data.test)

class.test = class.all[test.index]


######################
#SVM with soft margin#
######################
system.time({

  SVM.Margin.par = svm.margin.cv(dat.train = data.other, class.train = class.other, cost = c(0.01,0.02))

  })

system.time({
  
SVM.final.margin.mdodel = Train.SVM.margin(X = data.other,Y = class.other,cost = SVM.Margin.par$cost)

})

system.time({
  
SVM.margin.test.error = Test.SVM(SVM.final.margin.mdodel,val = data.test,class = class.test)

})

#################################
#SVM with soft margin and kernel#
#################################
system.time({
  
SVM.kernel.par = svm.margin.cv(dat.train = data.other, class.train = class.other, cost = 1, gamma = 0.0005)

})

system.time({
  
SVM.kernel.final.model = Train.SVM.kernel(X = data.other,Y = class.other,cost = SVM.kernel.par$cost,gamma = SVM.kernel.par$gamma)

})

system.time({
  
SVM.margin.test.error = Test.SVM(SVM.kernel.final.model,val = data.test,class = class.test)

})