########################################################
### AdaBoost with decision stump to reduce dimension ###
########################################################
### Author: Boxuan Zhao
### Project 3
### ADS spring 2017

#Selecte the classifier with the highest weights to reduce the dimension.

#Load revelant packages
list.of.packages <- c("reshape2", "ggplot2")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library("reshape2")
library("ggplot2")

y <- read.csv("D:/Columbia University/Spring2017-Applied Data Science/Project_3_Bz2290/spr2017-proj3-group-12/data/labels.csv", header = T)
oriData <- read.csv("D:/Columbia University/Spring2017-Applied Data Science/Project_3_Bz2290/spr2017-proj3-group-12/data/sift_features.csv", header = T)
X <- oriData
n_feature <- nrow(X)
n_case <- ncol(X)

#Chage class assignment for chicken to -1 and dogs to 1
y = y[1:2000,]
y = ifelse(y==0,-1,y)



####################################################TESTING values Ignore this################################################################################################
X = X[20:50,1:100]
y = y[1:100]

#Testing values
X.1 = X[1000:1039,1:100]
y.1 = y[1:100,]
n_feature.1 <- nrow(X.1)
n_case.1 <- ncol(X.1)

X.2 = X[20:50,1:130]
y.2 = y[1:130,]
n_feature.2 <- nrow(X.2)
n_case.2 <- ncol(X.2)

X.3 = X[20:50,1000:1200]
y.3 = y[1000:1200,]
n_feature.3 <- nrow(X.3)
n_case.3 <- ncol(X.3)

alpha.1 = 2
allPars.1 = matrix(c(1,1,1),ncol=3)

alpha.2 = c(2,3)
allPars.2 = matrix(c(1,1,1,2,1,-1),ncol=3)

alpha.3 = c(1,2,3)
allPars.3 = matrix(c(1,1,1,2,1,1,3,1,2),ncol = 3)
#######################################################################################################################################################



#Compose revelant functions:

###### agg_class 
#use the weighted sum of weak classifiers to produce the result
agg_class <- function(X, alpha, allPars){
  #X <- matrix(X, nrow = n_feature)
  class_predict <- matrix(apply(matrix(allPars, ncol = 3),MARGIN = 1,classify, X1 = X),nrow = ncol(X))
  #voting weight to each weak
  c_matrix <- matrix(rep(alpha,each = nrow(class_predict)) * class_predict, nrow = nrow(class_predict)) ###GOOD####
  
  #vote
  tmp = rowSums(c_matrix)
  c_hat = as.numeric(ifelse(tmp>0,1,-1))
  return(c_hat)
}


##### classify

#Implement decision stump to produce prediciton
classify <- function(X1,pars){
  #X1 <- matrix(X1, nrow = n_feature)
  label <- ifelse(X1[as.numeric(pars[1]),] > as.numeric(pars[2]), as.numeric(pars[3]), -as.numeric(pars[3]))
  return(label)
}


##### train

#Train a decision stump for data X with weights w and class y
train <- function(X,w,y){
  theta <- rep(NA, n_feature)
  m <- rep(NA, n_feature)
  err <- rep(NA, n_feature)
  
  for (j in 1:n_feature){
    # sort the data in the jth axis
    order_Xj <- order(X[j,])
    ordered_x <- as.numeric(X[j,order_Xj])
    ordered_y <- y[order_Xj]
    
    # use the unique value
    unique_x <- unique(ordered_x)
    unique_y <- as.numeric(tapply(w*ordered_y, factor(unlist(ordered_x)), sum))
    wcum <- cumsum(unique_y)
    index_max <- which.max(abs(wcum))
    
    # find the split point
    split_x <- unique_x[index_max] 
    theta[j] <- split_x
    m[j] <- ifelse(wcum[index_max]>0, -1, 1)
    
    # compute each feature's error
    err[j] <- sum(w * (y != classify(X, c(j, unlist(split_x), m[j]))))/sum(w)
  }
  # select the feature that yield minimum error
  index_par <- which.min(err)
  pars <- c(index_par, theta[index_par], m[index_par])
  return(pars)
}


AdaBoost.Cross <- function(B){
  k1 = 5
  folds <- sample(rep(1:5, each = n_case / k1)) 
  train_error <- matrix(NA, nrow = B, ncol = k1) 
  test_error <- matrix(NA, nrow = B, ncol = k1) 
  n_case_train = n_case * (1- 1/ k1)
  #Reshuffle the data
  Resample = sample(1:n_case,n_case,replace = FALSE)
  X = X[,Resample]
  y = y[Resample]
  for (j in 1:k1){
    # CV
    X.train <- X[,folds != j]
    X.test <- X[,folds == j]
    y.train <- y[folds != j]
    y.test <- y[folds == j]
    weights <- rep(1/n_case_train,n_case_train)
    
    # Standard steps for boosting
    alpha <- rep(NA, B)
    pars_m <- matrix(NA, nrow = B, ncol = 3) # pars of each weak classifier will be the rows
    for (b in 1:B){
      pars_m[b,] <- unlist(train(X.train, weights, y.train))
      
      ####################################################################################################################################
      #Thinking of a way to solve this...... When train error reahces zero, the alpha assigned to the classifier will be Inf, causing a bug
      
      #Orginal Line
      err_current <- sum(weights * (y.train != classify(X.train, pars_m[b,])))/sum(weights)
      
      #A line that will prevent this problem
      #err_current = 0.3 
      ###################################################################################################################################
      
      alpha[b] <- log((1-err_current)/err_current)
      weights <- weights * exp(alpha[b] * (y.train != classify(X.train, pars_m[b,])))
      
      # Train and predict test data
      label_train <- agg_class(X.train, alpha[1:b], pars_m[1:b,])
      train_error[b,j] <- sum(y.train != label_train) / length(y.train)
      label_test <- agg_class(X.test, alpha[1:b], pars_m[1:b,])
      
      # Test error
      test_error[b,j] <- sum(y.test != label_test) / length(y.test) 
    }
  }
  #take average of the 5 CV results
  train_error.ave <- rowMeans(train_error)
  test_error.ave <- rowMeans(test_error)
  return(list(Train_error = train_error.ave, Test_error = test_error.ave, Train_error_trace = train_error, Test_error_trace = test_error)) 
}




b_select = 1 # of weak learners
each_error <- AdaBoost.Cross(b_select)
error_df <- data.frame(b = 1:b_select, Train_error = each_error$Train_error, Test_error = each_error$Test_error)

#Plot the CV estimates of validation and train error over number of weak calssifiers
error_df <- melt(error_df, id = "b")
colnames(error_df) <- c("b","Type", "Error")
ggplot(data = error_df) +
  aes(x = b, y = Error, color = Type) +
  geom_line()
