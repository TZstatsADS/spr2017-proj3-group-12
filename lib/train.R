#########################################################
### Train a classification model with training images ###
#########################################################

### Author: Yuting Ma(train)
###         Boxuan Zhao(Train.SVM.margin and Train.SVM.kernel)
### Project 3
### ADS Spring 2016


train <- function(dat_train, label_train, par=NULL){
  
  ### Train a Gradient Boosting Model (GBM) using processed features from training images
  
  ### Input: 
  ###  -  processed features from images 
  ###  -  class labels for training images
  ### Output: training model specification
  
  ### load libraries
  library("gbm")
  
  ### Train with gradient boosting model
  if(is.null(par)){
    depth <- 3
  } else {
    depth <- par$depth
  }
  fit_gbm <- gbm.fit(x=dat_train, y=label_train,
                     n.trees=2000,
                     distribution="bernoulli",
                     interaction.depth=depth, 
                     bag.fraction = 0.5,
                     verbose=FALSE)
  best_iter <- gbm.perf(fit_gbm, method="test", plot.it = FALSE)

  return(list(fit=fit_gbm, iter=best_iter))
}

Train.SVM.margin = function(X,Y,cost)
{
  ### Train a Support Vector Machine (SVM) using processed features from training images with given cost
  
  ### Input: 
  ###  -  X : each row as an observation of the data
  ###  -  Y : a vector contains classes for each row of X
  ###  -  cost : specify the cost of the margin
  ### Output: training model specification
  model = svm(x=X,y=as.factor(Y),cost = cost,kernel = "linear")
  return(model)
}

Train.SVM.kernel = function(X,Y,cost,gamma)
{
  ### Train a Support Vector Machine (SVM) using processed features from training images with given cost and gamma
  
  ### Input: 
  ###  -  X : each row as an observation of the data
  ###  -  Y : a vector contains classes for each row of train.data
  ###  -  cost : specify the cost of the margin
  ###  -  gamma : specify the bandwidth
  ### Output: training model specification
  model = svm(x=X,y=as.factor(Y),cost = cost,gamma = gamma,type = "C",kernel = "radial")
  
  return(model)
}
