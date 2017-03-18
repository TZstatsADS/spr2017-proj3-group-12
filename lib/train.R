#########################################################
### Train a classification model with training images ###
#########################################################

### Author: Yuting Ma
### Project 3
### ADS Spring 2016


train <- function(dat_train, label_train, par=NULL,Ntrees = 2000){
  
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
                     n.trees=Ntrees,
                     distribution="bernoulli",
                     interaction.depth=depth, 
                     bag.fraction = 0.5,
                     verbose=FALSE)
  best_iter <- gbm.perf(fit_gbm, method="test", plot.it = FALSE)

  return(list(fit=fit_gbm, iter=best_iter))
}

train.SVM = function(kernel, train.data, train.class, cost, gamma)
{
  ### Train a Support Vector Machine (SVM) using processed features from training images
  
  ### Input: 
  ###  -  kernel : indicate whdther in include RBF kernel 
  ###  -  train.data : each row as an observation of the data
  ###  -  train.class : a vector contains classes for each row of train.data
  ###  -  cost : specify the cost of the margin
  ###  -  gamma : specify the bandwidth
  ### Output: training model specification
  if(kernel)
  {
    model = svm(x=train.data,y=as.factor(train.class),kernel = "radial",cost=cost,gamma=gamma,type = "C")
    
  }
  else if(!kernel)
  {
    model = svm(x=train.Data,y=as.factor(train.class),cost = cost,kernel = "linear")
    
  }
  return(model)
}
