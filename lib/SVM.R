#load porper packages
list.of.packages <- c("e107", "ggplot2")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(e1071)
library(ggplot2)

##################################Cross Validate to find the best parameter for the SVM model with givn trainning data set####################################

#SVM with soft margin

#Create 10 equally size folds
folds = cut(seq(1,nrow(data.other)),breaks=10,labels=FALSE)

validation.error.margin.final = c()
validation.error.margin.interm= c()
cost = 10^c(-5,-4,-3,-2,-1)
#Perform 10 fold cross validation
for(j in 1 :length(cost))
{
  for(i in 1:10){
    
    validation.Indexes <- which(folds==i,arr.ind=TRUE)
    validation.Data.margin <- data.other[validation.Indexes, ]
    train.Data.margin <- data.other[-validation.Indexes, ]
    train.class.margin = class.other[-validation.Indexes]
    validation.class.margin = class.other[validation.Indexes]
    #Train the model
    model = svm(x=train.Data.margin,y=as.factor(train.class.margin),cost = cost[j],kernel = "linear")
    #Prediction on the validation data
    pred <- predict(model,validation.Data.margin)
    #validation error for current iteration with current cost
    validation.error.margin.interm[i] = mean(pred != validation.class.margin)  
    
  }
  #Obtain the validation error for the current cost
  validation.error.margin.final[j] = mean(validation.error.margin.interm)
}
#A function of margin parameter in the linear case
data.plot = data.frame(cost = cost, error = validation.error.margin.final)
ggplot(data = data.plot) + geom_point(mapping = aes(x=data.plot$cost,y=data.plot$error))+
  labs(x="Margin",y="Misclassification rate",title = "Cross-validation estimates in Linear Case")+
  geom_line(mapping = aes(x=data.plot$cost,y=data.plot$error))

#Hence the parameter is:
margin.cost = cost[which.min(data.plot$error)]



#SVM with soft margin and kernel
#Again, we perform 10 - fold cross validation
cost = 10^(1:2)
gamma = c(0.005,0.015,0.01)

val.par.frame = data.frame(cost = as.vector(mapply(rep,cost,length(gamma))), gamma = rep(gamma,length(cost)), error = NA)

validation.error.m.k.i = c()


  for(i in 1:nrow(val.par.frame))
  {
    for(j in 1:10)
    {
      
      validation.Index = which(folds == j, arr.ind = TRUE)
      validation.data.m.k = data.other[validation.Index,]
      train.data.m.k = data.other[-validation.Index,]
      validation.class.m.k = class.other[validation.Index]
      train.class.m.k =  class.other[-validation.Index]
      #Train SVM model with current cost and current gamma at the ith iteration
      model = svm(x=train.data.m.k,y=as.factor(train.class.m.k),kernel = "radial",cost=val.par.frame$cost[i],gamma=val.par.frame$gamma[i],type = "C")
      #Orediction on validation data with current cost and current gamma at current iteration
      pred = predict(model,validation.data.m.k)
      #Validaiton error at this iteration with current gamma and cost
      validation.error.m.k.i[i] = mean(pred != validation.class.m.k)
      
    }
    val.par.frame$error[i] = mean(validation.error.m.k.i)
  }



#A function of margin parameter in the non-linear case
ggplot(data = val.par.frame)+
  geom_point(mapping =aes(x=val.par.frame$cost,y=val.par.frame$error,col=val.par.frame$gamma))+
  labs(x="Margin",y="Misclassification rate",title = "Cross validation estimates in non-Linear Case")+
  geom_line(mapping = aes(x=val.par.frame$cost,y=val.par.frame$error,col=val.par.frame$gamma))+
  scale_color_discrete(name ="Kernel BandWidth")

#Hence the parameters are
#For cost:
kernel.cost = val.par.frame$cost[which.min(val.par.frame$error)]
#For gamma:
kernel.gamma = as.numeric(as.character(val.par.frame$gamma[which.min(val.par.frame$error)]))

########################################Train Both SVM with chosen parameters on all sets except test set#####################################################
#For svm with only margin
svm.m.after = svm(x = data.other, y = as.factor(class.other), cost = margin.cost, kernel  = "linear")

#For svm with margin and RBF kernel
svm.m.k.after = svm(x = data.other,y = as.factor(class.other), cost = kernel.cost, gamma = kernel.gamma, type = "C")

##################################################Compute misclassification rate on test data#################################################################
#For SVM with only margin
pred.margin <- predict(svm.m.after,data.test)

#Misclassification rate for SVM with only margin
mean(pred.margin != class.test)

#For svm with margin and RBF kernel
pred.margin.kernel <- predict(svm.m.k.after,data.test)

#Misclassification rate for SVM with margin and RBF kernel
mean(pred.margin.kernel != class.test)
