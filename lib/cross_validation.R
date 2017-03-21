########################
### Cross Validation ###
########################

### Author: Yuting Ma(cv.function)
###         Boxuan Zhao(svm.margin.cv and svm.kernel.cv)
### Project 3
### ADS Spring 2016


cv.function <- function(X.train, y.train, d, K){
  
  n <- length(y.train)
  n.fold <- floor(n/K)
  s <- sample(rep(1:K, c(rep(n.fold, K-1), n-(K-1)*n.fold)))  
  cv.error <- rep(NA, K)
  
  for (i in 1:K){
    train.data <- X.train[s != i,]
    train.label <- y.train[s != i]
    test.data <- X.train[s == i,]
    test.label <- y.train[s == i]
    
    par <- list(depth=d)
    fit <- train(train.data, train.label, par)
    pred <- test(fit, test.data)  
    cv.error[i] <- mean(pred != test.label)  
    
  }			
  return(c(mean(cv.error),sd(cv.error)))
  
}

svm.margin.cv = function(dat.train, class.train, cost)
{
  ### Use Cross validation to find the best cost parameter(if more than one provided)
  
  ### Input: 
  ###  -  dat.train : train data set with each row as an observation
  ###  -  class.train : a vector contains classes for each row of X
  ###  -  cost : specify the cost of the margin
  
  ### Output: a list contains the best cost parameter, along with the coresponding error, 
  ###         as well as a data frame conclude all cost values and error rates.
  
  val.err.cost.interm = numeric(5)
  val.err.cost.f = numeric(length(cost))
  folds = cut(seq(1,nrow(dat.train)),breaks=5,labels=FALSE)
  #Perform 5 fold cross validation
  for(j in 1 :length(cost))
  { 
    #j = 1
    for(i in 1:5){
      
      val.Indexes <- which(folds==i,arr.ind=TRUE)
      val.Data <- dat.train[val.Indexes, ]
      train.Data <- dat.train[-val.Indexes, ]
      train.class = class.train[-val.Indexes]
      val.class = class.train[val.Indexes]
      #Train the model
      model = svm(x = train.Data, y = as.factor(train.class), cost = cost[j], kernel = "linear")
      #Prediction on the validation data
      pred <- predict(model,val.Data)
      #validation error for current iteration with current cost
      val.err.cost.interm[i] = mean(pred != val.class)  
      
    }
    #Obtain the validation error for the current cost
    val.err.cost.f[j] = mean(val.err.cost.interm)
  }
  svm.data.margin = data.frame(cost = cost, cv.error = val.err.cost.f)
  
  #For cost
  margin.cost = svm.data.margin$cost[which.min(svm.data.margin$cv.error)]
  
  return(list(cost = margin.cost, cv.error = min(svm.data.margin$cv.error), frame = svm.data.margin))
}

svm.kernel.cv = function(dat.train, class.train, cost, gamma)
{
  ### Use Cross validation to find the best cost and gamma parameter(if more than one provided for both)
  
  ### Input: 
  ###  -  dat.train : train data set with each row as an observation
  ###  -  class.train : a vector contains classes for each row of X
  ###  -  cost : specify the cost of the margin
  ###  -  gamma: specify the bandwidth for RBF kernel
  
  ### Output: a list contains the best cost and gamma parameter, 
  ###         along with the coresponding error, as well as a data frame conclude all cost and gamma values and error rates.
  
  folds = cut(seq(1,nrow(dat.train)),breaks=5,labels=FALSE)
  val.par.frame = data.frame(cost = as.vector(mapply(rep,cost,length(gamma))), gamma = rep(gamma,length(cost)), error = NA)
  val.err.i = c()
  for(i in 1:nrow(val.par.frame))
  {
    #i = 1
    for(j in 1:5)
    {

      val.Index = which(folds == j, arr.ind = TRUE)
      val.data = dat.train[val.Index,]
      train.data = dat.train[-val.Index,]
      val.class = class.train[val.Index]
      train.class =  class.train[-val.Index]
      #Train SVM model with current cost and current gamma at the ith iteration
      #model = Train.SVM.kernel(X=train.data.m.k,Y=train.class.m.k,cost=val.par.frame$cost[i],gamma = val.par.frame$gamma[i])
      model = svm(x=train.data,y=as.factor(train.class),cost = val.par.frame$cost[i],gamma = val.par.frame$gamma[i],type = "C",kernel = "radial")
      #Prediction on validation data with current cost and current gamma at current iteration
      pred = predict(model,val.data)
      #Validaiton error at this iteration with current gamma and cost
      val.err.i[j] = mean(pred != val.class)
      
    }
    val.par.frame$error[i] = mean(val.err.i)
  }
  
  #For cost
  kernel.cost = val.par.frame$cost[which.min(val.par.frame$error)]
  #For gamma:
  kernel.gamma = as.numeric(as.character(val.par.frame$gamma[which.min(val.par.frame$error)]))
  
  return(list(cost = kernel.cost, gamma = kernel.gamma, cv.error = min(val.par.frame$error), frame = val.par.frame))
}

