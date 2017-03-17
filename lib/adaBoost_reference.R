## 2. Boosting

y <- unlist(read.table("D:/Columbia University/Spring2017-Statistical Machine Learning/Homework/uspscl.txt", header = F))
oriData <- read.table("D:/Columbia University/Spring2017-Statistical Machine Learning/Homework/uspsdata.txt", header = F) 
X <- t(oriData)
n_feature <- nrow(X)
n_case <- ncol(X)


###### AdaBoost
AdaBoost <- function(B, X_mat){
  weights <- rep(1/n_case,n_case)
  alpha <- rep(NA, B)
  pars_m <- matrix(NA, nrow = B, ncol = 3) # weak classifier
  for (b in 1:B){
    pars_m[b,] <- train(X, weights, y)
    err <- sum(weights * (y != classify(X, pars_m[b,])))/sum(weights)
    alpha[b] <- log((1-err)/err)
    weights <- weights * exp(alpha[b] * (y != classify(X, pars_m[b,])))
  }
  label_forb <- apply(matrix(X_mat, nrow = 256), 2, agg_class, alpha, pars_m)
  return(label_forb)
}


###### agg_class
agg_class <- function(X, alpha, allPars){
  X <- matrix(X, nrow = n_feature)
  class_predict <- matrix(apply(matrix(allPars, ncol = 3),MARGIN = 1,classify, X1 = X),nrow = ncol(X))
  #voting weight to each weak
  c_matrix <- matrix(rep(alpha,each = nrow(class_predict)) * class_predict, nrow = nrow(class_predict))
  
  #vote
  tmp = rowSums(c_matrix)
  c_hat = as.numeric(ifelse(tmp>0,1,-1))
  return(c_hat)
}


##### classify 
classify <- function(X1,pars){
  X1 <- matrix(X1, nrow = n_feature)
  label <- ifelse(X1[pars[1],] > pars[2], pars[3], -(pars[3]))
  return(label)
}


##### train
train <- function(X,w,y){
  theta <- rep(NA, n_feature)
  m <- rep(NA, n_feature)
  err <- rep(NA, n_feature)
  
  for (j in 1:n_feature){
    # sort the data in the jth axis
    order_Xj <- order(X[j,])
    ordered_x <- X[j,order_Xj]
    ordered_y <- y[order_Xj]
    
    # use the unique value
    unique_x <- unique(ordered_x)
    unique_y <- as.numeric(tapply(w*ordered_y, factor(ordered_x), sum))
    wcum <- cumsum(unique_y)
    index_max <- which.max(abs(wcum))
    
    # find the split point
    split_x <- unique_x[index_max] 
    theta[j] <- split_x
    m[j] <- ifelse(wcum[index_max]>0, -1, 1)
    
    # compute each feature's error
    err[j] <- sum(w * (y != classify(X, c(j, split_x, m[j]))))
  }
  # select the feature that yield minimum error
  index_par <- which.min(err)
  pars <- c(index_par, theta[index_par], m[index_par])
  return(pars)
}
```

run boosting on usps data with CV
```{r}
AdaBoost.Cross <- function(B){
  k1 = 5
  folds <- sample(rep(1:5, each = n_case / k1)) 
  train_error <- matrix(NA, nrow = B, ncol = k1) 
  test_error <- matrix(NA, nrow = B, ncol = k1) 
  n_case_train = n_case * (1- 1/ k1)
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
      pars_m[b,] <- train(X.train, weights, y.train)
      err_current <- sum(weights * (y.train != classify(X.train, pars_m[b,])))/sum(weights)
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


library("reshape2")
library("ggplot2")

b_select = 70 #70 weak learners
each_error <- AdaBoost.Cross(b_select)
error_df <- data.frame(b = 1:b_select, Train_error = each_error$Train_error, Test_error = each_error$Test_error)

error_df <- melt(error_df, id = "b")
colnames(error_df) <- c("b","Type", "Error")
ggplot(data = error_df) +
  aes(x = b, y = Error, color = Type) +
  geom_line()



##################################


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


y <- unlist(read.table("D:/Columbia University/Spring2017-Statistical Machine Learning/Homework/uspscl.txt", header = F))
oriData <- read.table("D:/Columbia University/Spring2017-Statistical Machine Learning/Homework/uspsdata.txt", header = F) 
X <- t(oriData)
n_feature <- nrow(X)
n_case <- ncol(X)


#Chage class assignment for chicken to -1 and dogs to 1
y = y[1:2000,]
y = ifelse(y==0,-1,y)

#Compose revelant functions:
###### agg_class
agg_class <- function(X, alpha, allPars){
  X <- matrix(X, nrow = n_feature)
  class_predict <- matrix(apply(matrix(allPars, ncol = 3),MARGIN = 1,classify, X1 = X),nrow = ncol(X))
  #voting weight to each weak
  c_matrix <- matrix(rep(alpha,each = nrow(class_predict)) * class_predict, nrow = nrow(class_predict))
  
  #vote
  tmp = rowSums(c_matrix)
  c_hat = as.numeric(ifelse(tmp>0,1,-1))
  return(c_hat)
}


##### classify 
classify <- function(X1,pars){
  X1 <- matrix(X1, nrow = n_feature)
  label <- ifelse(X1[pars[1],] > pars[2], pars[3], -(pars[3]))
  return(label)
}


##### train
train <- function(X,w,y){
  theta <- rep(NA, n_feature)
  m <- rep(NA, n_feature)
  err <- rep(NA, n_feature)
  
  for (j in 1:n_feature){
    # sort the data in the jth axis
    order_Xj <- order(X[j,])
    ordered_x <- X[j,order_Xj]
    ordered_y <- y[order_Xj]
    
    # use the unique value
    unique_x <- unique(ordered_x)
    unique_y <- as.numeric(tapply(w*ordered_y, factor(ordered_x), sum))
    wcum <- cumsum(unique_y)
    index_max <- which.max(abs(wcum))
    
    # find the split point
    split_x <- unique_x[index_max] 
    theta[j] <- split_x
    m[j] <- ifelse(wcum[index_max]>0, -1, 1)
    
    # compute each feature's error
    err[j] <- sum(w * (y != classify(X, c(j, split_x, m[j]))))
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
      pars_m[b,] <- train(X.train, weights, y.train)
      err_current <- sum(weights * (y.train != classify(X.train, pars_m[b,])))/sum(weights)
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




b_select = 30 #100 weak learners
each_error <- AdaBoost.Cross(b_select)
error_df <- data.frame(b = 1:b_select, Train_error = each_error$Train_error, Test_error = each_error$Test_error)

error_df <- melt(error_df, id = "b")
colnames(error_df) <- c("b","Type", "Error")
ggplot(data = error_df) +
  aes(x = b, y = Error, color = Type) +
  geom_line()
