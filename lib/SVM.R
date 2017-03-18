#################
### SVM model ###
#################
### Author: Boxuan Zhao
### Project 3
### ADS spring 2017


#load porper packages
list.of.packages <- c("e107", "ggplot2")

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

write.csv(new_feature, file="./data/sift.feature.New.csv")

#feature.new = function(dat)
#{
#  variation = apply(dat,1,sd) 
  
  #Thereshold value for known varaince
 # thereshold = summary(variation)[2]
  
#  dat = dat[which(variation > thereshold),]
  
 # data.dog = dat[,1001:2000]
  #data.chicken = dat[,1:1000]
  
  #avg.dog = apply(data.dog,1,mean)
  #avg.chicken = apply(data.chicken,1,mean)
  
  #avg.difference = abs(avg.dog - avg.chicken)
  
  #large = which(avg.difference > summary(avg.difference)[3])
  
  #dat = dat[large,]
  
  #return(dat)
#}

system.time({
  
test = feature.new(data.all)

})
###############################################################################################################################

#################
#Data Processing#
#################

data.all = data.feature

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

#STARTS:

#Crosvalidation to find the best parameters

#Create 5 equally size folds
system.time({
  
folds = cut(seq(1,nrow(data.other)),breaks=5,labels=FALSE)

val.err.cost.f = c()
val.err.cost.interm = c()
cost = c(0.01)

#Perform 5 fold cross validation
#for(j in 1 :length(cost))
#{
j = 1
  for(i in 1:5){
    val.Indexes <- which(folds==i,arr.ind=TRUE)
    val.Data <- data.other[val.Indexes, ]
    train.Data <- data.other[-val.Indexes, ]
    train.class = class.other[-val.Indexes]
    val.class = class.other[val.Indexes]
    #Train the model
    model = svm(x=train.Data,y=as.factor(train.class),cost = cost[j],kernel = "linear")
    #Prediction on the validation data
    pred <- predict(model,val.Data)
    #validation error for current iteration with current cost
    val.err.cost.interm[i] = mean(pred != val.class)  
    
  }
  #Obtain the validation error for the current cost
  val.err.cost.f[j] = mean(val.err.cost.interm)
#}

})

#A function of margin parameter in the linear case
data.plot = data.frame(cost = cost, error = val.err.cost.f)

#ggplot(data = data.plot) + geom_point(mapping = aes(x=data.plot$cost,y=data.plot$error))+
 # labs(x="Margin",y="Misclassification rate",title = "Cross-validation estimates in Linear Case")+
  #geom_line(mapping = aes(x=data.plot$cost,y=data.plot$error))

#Hence the parameter is:
margin.cost = cost[which.min(data.plot$error)]

#For svm with only margin

system.time({
  
  svm.m.after = svm(x = data.other, y = as.factor(class.other), cost = margin.cost, kernel  = "linear")

})

#For SVM with only margin

system.time({

  pred.margin <- predict(svm.m.after,data.test)

  #Misclassification rate for SVM with only margin
  mean(pred.margin != class.test)

})

#ENDS

#################################
#SVM with soft margin and kernel#
#################################
#Again, we perform 10 - fold cross validation
cost = c(1)
gamma = c(0.0005,0.001,0.0015) #0.4966667
#gamma = c(0.02,0.03,0.04) #0.4966667
#gamma = c(0.1,0.2,0.3) #0.4966667
val.par.frame = data.frame(cost = as.vector(mapply(rep,cost,length(gamma))), gamma = rep(gamma,length(cost)), error = NA)

val.err.m.k.i = c()


  for(i in 1:nrow(val.par.frame))
  {
    #for(j in 1:5)
    #{
      j = 1
      val.Index = which(folds == j, arr.ind = TRUE)
      val.data.m.k = data.other[val.Index,]
      train.data.m.k = data.other[-val.Index,]
      val.class.m.k = class.other[val.Index]
      train.class.m.k =  class.other[-val.Index]
      #Train SVM model with current cost and current gamma at the ith iteration
      model = svm(x=train.data.m.k,y=as.factor(train.class.m.k),kernel = "radial",cost=val.par.frame$cost[i],gamma=val.par.frame$gamma[i],type = "C")
      #Orediction on validation data with current cost and current gamma at current iteration
      pred = predict(model,val.data.m.k)
      #Validaiton error at this iteration with current gamma and cost
      val.err.m.k.i[i] = mean(pred != val.class.m.k)
      
    #}
    val.par.frame$error[i] = mean(val.err.m.k.i)
  }

#Generate data plot
data.plot=data.frame(cost = val.par.frame$cost, error = val.par.frame$error, gamma = as.factor(val.par.frame$gamma))

#A function of margin parameter in the non-linear case
#ggplot(data = val.par.frame)+
 # geom_point(mapping =aes(x=data.plot$cost,y=data.plot$error,col=data.plot$gamma))+
  #labs(x="Margin",y="Misclassification rate",title = "Cross validation estimates in non-Linear Case")+
  #geom_line(mapping =aes(x=data.plot$cost,y=data.plot$error,col=data.plot$gamma))+
  #scale_color_discrete(name ="Kernel BandWidth")

#Hence the parameters are
#For cost:
kernel.cost = val.par.frame$cost[which.min(val.par.frame$error)]
#For gamma:
kernel.gamma = as.numeric(as.character(val.par.frame$gamma[which.min(val.par.frame$error)]))


#For svm with margin and RBF kernel
system.time({
  
svm.m.k.after = svm(x = data.other,y = as.factor(class.other), cost = kernel.cost, gamma = kernel.gamma, type = "C")

)}

#For svm with margin and RBF kernel
system.time({
  
pred.margin.kernel <- predict(svm.m.k.after,data.test)


#Misclassification rate for SVM with margin and RBF kernel
mean(pred.margin.kernel != class.test)

)}