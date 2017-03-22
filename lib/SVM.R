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
source("../lib/train.R")
source("../lib/test.R")
source("../lib/cross_validation.R")

dat.ori.train = read.csv("../data/sift_ori_train.csv",header = T)

dat.ori.train = t(dat.ori.train)


dat.ori.test = read.csv("../data/sift_ori_test.csv",header = T)

dat.ori.test = t(dat.ori.test)


dat.simp.train = read.csv("../data/sift_simp_train.csv",header = T)

dat.simp.train = t(dat.simp.train)


dat.simp.test = read.csv("../data/sift_simp_test.csv",header = T)

dat.simp.test = t(dat.simp.test)


class.train = read.csv("../data/labels_train.csv",header = T)

class.train = class.train[,1]


class.test = read.csv("../data/labels_test.csv",header = T)

class.test = class.test[,1]


dat.simp.gray.train = read.csv("../data/sift_simp_gray_train.csv",header = T)

dat.simp.gray.train = t(dat.simp.gray.train)


dat.simp.gray.test = read.csv("../data/sift_simp_gray_test.csv",header = T)

dat.simp.gray.test = t(dat.simp.gray.test)


dat.ori.gray.train = read.csv("../data/sift_ori_gray_train.csv",header = T)

dat.ori.gray.train = t(dat.ori.gray.train)


dat.ori.gray.test = read.csv("../data/sift_ori_gray_test.csv",header = T)

dat.ori.gray.test = t(dat.ori.gray.test)

####################
#Do not Use feature#
####################

#data.all = data.all[which(apply(data.all,1,sd)>summary(apply(data.all,1,sd))[2]),]

############################################
#SVM with soft margin on Simplified Feature#
############################################


#> cv.simp1
#用户    系统    流??? 
#1117.16   10.28 1260.41 
#> train.simp1
#用户  系统  流??? 
#63.11  0.34 67.17 
#> pred.simp1
#用户 系统 流??? 
#6.63 0.13 7.56 




#> SVM.Margin.par.simp
#$cost
#[1] 0.001

#$cv.error
#[1] 0.288

#$frame
#cost  cv.error
#1 0.000100000 0.7866667
#2 0.001000000 0.2880000
#3 0.003162278 0.3000000
#4 0.010000000 0.3080000
#5 0.100000000 0.3080000

#> SVM.margin.test.error.simp
#[1] 0.2
 

cv.simp1 = system.time({

  SVM.Margin.par.simp = svm.margin.cv(dat.train = dat.simp.train, class.train = class.train, cost = 10^c(-4,-3,-2.5,-2,-1))

  })

train.simp1 = system.time({
  
SVM.final.margin.model.simp = Train.SVM.margin(X = dat.simp.train,Y = class.train,cost = SVM.Margin.par.simp$cost)

})

pred.simp1 = system.time({
  
SVM.margin.test.error.simp = Test.SVM(SVM.final.margin.model.simp,val = dat.simp.test,class = class.test)

})

#data.plot.1 = data.frame(cost = c(0.0001,0.001,0.003162278,0.01,0.1),cv.error=c(0.7866667,0.288,0.3,0.308,0.308),test.error=NA)

#for(i in 1 : length(data.plot.1$cost))
#{
 # data.plot.1$test.error[i] = Test.SVM(Train.SVM.margin(X = dat.simp.train,Y = class.train,cost = data.plot.1$cost[i]),val = dat.simp.test,class = class.test)
#}

#data.plot.22 = data.frame(cost=c(0.0001,0.001,0.003162278,0.01,0.1,0.0001,0.001,0.003162278,0.01,0.1),error = c(data.plot.1$cv.error,data.plot.1$test.error),class=c(rep("Validation Error",5),rep("Test Error",5)))

#jpeg(filename = "../figs/LinearSVM_Simplified Sift feature and Gray feature.jpg")

#ggplot(data=data.plot.22)+
 # geom_point(mapping = aes(x=data.plot.22$cost,y=data.plot.22$error,col=data.plot.22$class))+
#  geom_line(mapping = aes(x=data.plot.22$cost,y=data.plot.22$error,col=data.plot.22$class))+
 # labs(x="Cost",y="Error rates",title="Linear SVM on SImplified Sift feature")+
  #scale_color_discrete(name ="Error Type")

SVM.Margin.par.simp

SVM.margin.test.error.simp
##########################################################################################################################
##############################################################
#SVM with soft margin on Simplified Feature and gray features#
##############################################################

#> cv.simp2
#用户    系统    流??? 
#1226.29   10.75 1362.59 
#> train.simp2
#用户  系统  流??? 
#58.64  0.69 67.94 
#> pred.simp2
#用户 系统 流??? 
#6.33 0.07 7.97 



#> SVM.Margin.par.simp.gray
#$cost
#[1] 0.001

#$cv.error
#[1] 0.19

#$frame
#cost  cv.error
#1 0.000100000 0.4053333
#2 0.001000000 0.1900000
#3 0.003162278 0.1913333
#4 0.010000000 0.1940000
#5 0.100000000 0.1940000

#> SVM.margin.test.error.simp.gray
#[1] 0.132
#data.plot.2 = data.frame(cost=c(0.0001,0.001,0.003162278,0.01,0.1),cv.error=c(0.4053333,0.19,0.1913333,0.194,0.194),test.error=NA)
#for(i in 1 : length(data.plot.2$cost))
#{
#  data.plot.2$test.error[i] = Test.SVM(Train.SVM.margin(X = dat.simp.gray.train,Y = class.train,cost = data.plot.2$cost[i]),val = dat.simp.gray.test,class = class.test)
#}
#data.plot.22 = data.frame(cost=c(0.0001,0.001,0.003162278,0.01,0.1,0.0001,0.001,0.003162278,0.01,0.1),error = c(data.plot.2$cv.error,data.plot.2$test.error),class=c(rep("Validation Error",5),rep("Test Error",5)))

#jpeg(filename = "../figs/LinearSVM_Simplified Sift feature and Gray feature.jpg")

#ggplot(data=data.plot.22)+
 # geom_point(mapping = aes(x=data.plot.22$cost,y=data.plot.22$error,col=data.plot.22$class))+
#  geom_line(mapping = aes(x=data.plot.22$cost,y=data.plot.22$error,col=data.plot.22$class))+
 # labs(x="Cost",y="Error rates",title="Linear SVM on SImplified Sift feature")+
  #scale_color_discrete(name ="Error Type")

cv.simp2 = system.time({
  
  SVM.Margin.par.simp.gray = svm.margin.cv(dat.train = dat.simp.gray.train, class.train = class.train, cost = 10^c(-5,-6,1,2,3))
  
})

train.simp2 = system.time({
  
  SVM.final.margin.model.simp.gray = Train.SVM.margin(X = dat.simp.gray.train,Y = class.train,cost = SVM.Margin.par.simp.gray$cost)
  
})

pred.simp2 = system.time({
  
  SVM.margin.test.error.simp.gray = Test.SVM(SVM.final.margin.model.simp.gray,val = dat.simp.gray.test,class = class.test)
  
})

SVM.Margin.par.simp.gray

SVM.margin.test.error.simp.gray


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


cv.ori1 = system.time({
  
  SVM.Margin.par.ori = svm.margin.cv(dat.train = dat.ori.train, class.train = class.train, cost = 10^c(-4,-3,-2.5,-2,-1))
  
})

train.ori1 = system.time({
  
  SVM.final.margin.mdoel.ori = Train.SVM.margin(X = dat.ori.train,Y = class.train,cost = SVM.Margin.par.ori$cost)
  
})

pred.ori1 = system.time({
  
  SVM.margin.test.error.ori = Test.SVM(SVM.final.margin.mdoel.ori,val = dat.ori.test,class = class.test)
  
})

SVM.Margin.par.ori

SVM.margin.test.error.ori


################################################################
#SVM with soft margin on Original Sift Feature and gray feature#
################################################################


cv.ori2 = system.time({
  
  SVM.Margin.par.ori.gray = svm.margin.cv(dat.train = dat.ori.gray.train, class.train = class.train, cost = 10^c(-4,-3,-2.5,-2,-1))
  
})

train.ori2 = system.time({
  
  SVM.final.margin.mdoel.ori.gray = Train.SVM.margin(X = dat.ori.gray.train,Y = class.train,cost = SVM.Margin.par.ori.gray$cost)
  
})

pred.ori2 = system.time({
  
  SVM.margin.test.error.ori.gray = Test.SVM(SVM.final.margin.mdoel.ori.gray,val = dat.ori.gray.test,class = class.test)
  
})

SVM.Margin.par.ori.gray

SVM.margin.test.error.ori.gray











#####################################################################################################################################################

############################################################
#SVM with soft margin and kernel on Simplified sift Feature#
############################################################

kernel.cv.simp.3 = system.time({
  
SVM.kernel.par.simp = svm.kernel.cv(dat.train = dat.simp.train, class.train = class.train, cost = 1, gamma = 0.5)

})

kernel.train.simp.3 = system.time({
  
SVM.kernel.final.model.simp = Train.SVM.kernel(X = dat.simp.train,Y = class.train,cost = SVM.kernel.par$cost,gamma = SVM.kernel.par$gamma)

})

kernel.pred.simp.3 = system.time({
  
SVM.kernel.test.error.simp = Test.SVM(SVM.kernel.final.model,val = dat.simp.test,class = class.test)

})

SVM.kernel.par.simp

SVM.kernel.test.error.simp

####################################################################################################################################
##########################################################
#SVM with soft margin and kernel on Original sift Feature#
##########################################################

kernel.cv.ori = system.time({
  
  SVM.kernel.par.ori = svm.kernel.cv(dat.train = dat.ori.train, class.train = class.train, cost = 10^( -4: -1), gamma = 2^(seq(-10, 0, 0.5)))
  
})

kernel.train.ori = system.time({
  
  SVM.kernel.final.model.ori = Train.SVM.kernel(X = dat.ori.train,Y = class.train,cost = SVM.kernel.par$cost,gamma = SVM.kernel.par$gamma)
  
})

kernel.pred.ori = system.time({
  
  SVM.kernel.test.error.ori = Test.SVM(SVM.kernel.final.model,val = dat.ori.test,class = class.test)
  
})

SVM.kernel.par.ori

SVM.kernel.test.error.ori
