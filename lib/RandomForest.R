#setwd("C:/Users/Vikas/OneDrive/Cloud Workspace/Documents/Columbia/Senior/Applied Data Science/Project 3/spr2017-proj3-group-12/lib")

#install.packages("randomForest")
library("randomForest")

#Function used to explore and validate various random forest parameters. From this, it was determined that no more than 500 trees are needed.
RandomForestExploration = function()
{
  image_features = t(read.csv("../data/sift_features.csv"))
  image_labels = unlist(read.csv("../data/labels.csv"))
  image_rf = randomForest(x = image_features, y = as.factor(image_labels), ntree = 1500)
  best_ntree = which.min(image_rf$err.rate[,"OOB"])
  err_rate = c(image_rf$err.rate[100, "OOB"], image_rf$err.rate[500, "OOB"], image_rf$err.rate[1000, "OOB"], image_rf$err.rate[1500, "OOB"])
  names(err_rate) = c("100", "500", "1000", "1500")
  jpeg(filename = "../figs/RandomForestErrorOriginalFeatures.jpg")
  plot(x = c(1:1500), y = image_rf$err.rate[,"OOB"], main = "Validation Error for Random Forest", xlab = "Number of Trees", ylab = "Validation Error")
  dev.off()
}

trainRandomForest = function(feature_filename, labels_filename, full_feature = FALSE)
{
  t = proc.time()
  image_features = t(read.csv(feature_filename))
  image_labels = unlist(read.csv(labels_filename))
  image_rf = randomForest(x = image_features, y = as.factor(image_labels), ntree = 500)
  train_time = (proc.time() - t)[3]
  cat("Elapsed time for Training Random Forest with 500 trees is ", train_time, " seconds \n")
  err_rate = image_rf$err.rate[500, "OOB"]
  cat("Validation Error rate for Random Forest with 500 trees is", err_rate, "\n") #.2895 for full feature set
  filename = "../output/RFModifiedFeature.RData"
  if(full_feature == TRUE)
  {
    filename = "../output/RFFullFeature.RData"
  }
  save(image_rf, file = filename)
  return(image_rf)
}

testRandomForest = function(rf_object, features_filename, full_feature = FALSE)
{
  t = proc.time()
  image_features = t(read.csv(features_filename))
  rf_predict = as.vector(predict(rf_object, image_features))
  test_time = (proc.time() - t)[3]
  cat("Elapsed prediction time for  Random Forest with 500 trees is ", test_time, " seconds \n")
  filename = "../output/RFModifiedPredictions.csv"
  if(full_feature == TRUE)
  {
    filename = "../output/RFFullFeaturePredictions.csv"
  }
  write.csv(rf_predict, file = filename)
  return(rf_predict)
}
