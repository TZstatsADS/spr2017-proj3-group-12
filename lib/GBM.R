setwd("C:/Users/Vikas/OneDrive/Cloud Workspace/Documents/Columbia/Senior/Applied Data Science/Project 3/spr2017-proj3-group-12/lib")

install.packages("gbm")
install.packages("caret")

library("gbm")
library("caret")

ExploreGBM = function(feature_filename, labels_filename)
{
  image_features = t(read.csv("../data/sift_features.csv"))
  image_labels = unlist(read.csv("../data/labels.csv"))
  parameters = expand.grid(interaction.depth = 1, n.trees = c(100, 500, 1000), shrinkage = c(.001, .01, .1), n.minobsinnode = 10)
  fitControl = trainControl(method = "repeatedcv", number = 5, repeats = 1)
  tune_gbm = train(image_features, as.factor(image_labels), method = "gbm", tuneGrid = parameters, trControl = fitControl)
  #Best Model has shrinkage .1, ntrees = 1000
  jpeg("../figs/GBMAccuracypnFullData.jpeg")
  plot(tune_gbm$results$shrinkage[7:9], tune_gbm$results$Accuracy[7:9], type = "b", col = "red", lty = 2, xlab = "shrinkage", ylab = "accuracy", main = "Accuracy vs. Shrinkage", ylim = c(0.65,0.78), pch = 1)
  points(tune_gbm$results$shrinkage[4:6], tune_gbm$results$Accuracy[4:6], type = "b", col = "blue", lty = 2, pch = 2)
  points(tune_gbm$results$shrinkage[1:3], tune_gbm$results$Accuracy[1:3], type = "b", col = "purple", lty = 2, pch = 3)
  legend("topleft", c("1000", "500", "100"), col = c("red","blue","purple"), pch = c(1,2,3), title = "Num. Stumps")
  dev.off()
  
}

TrainGBM = function(feature_filename,labels_filename, param_trees = 500, full_feature = FALSE)
{
  t = proc.time()
  image_features = t(read.csv(feature_filename))
  image_labels = unlist(read.csv(labels_filename))
  param_shrinkage = 0.1
  parameters = expand.grid(interaction.depth = 1, n.trees = param_trees, shrinkage = param_shrinkage, n.minobsinnode = 10)
  fitControl = trainControl(method = "repeatedcv", number = 5, repeats = 1)
  tune_gbm = train(image_features, as.factor(image_labels), method = "gbm", tuneGrid = parameters, trControl = fitControl)
  train_time = (proc.time() - t)[3]
  cat("Elapsed prediction time for  GBM with ", param_trees, "trees and shrinkage 0.1 is " , train_time, " seconds \n")
  filename = "../output/GBMModifiedFeature.RData"
  if(full_feature == TRUE)
  {
    filename = "../output/GBMFullFeature.RData"
  }
  save(tune_gbm, file = filename)
  return(tune_gbm)
  
}

TestGBM = function(gbm_model, feature_filename, param_trees = 500, full_feature = FALSE)
{
  t = proc.time()
  image_features = t(read.csv(feature_filename))
  gbm_predict = predict(tune_gbm, image_features, n.trees = param_trees)
  test_time = (proc.time() - t)[3]
  cat("Elapsed prediction time for  GBM with ", param_trees, " trees is ", test_time, " seconds \n")
  filename = "../output/GBMModifiedPredictions.csv"
  if(full_feature == TRUE)
  {
    filename = "../output/GBMFullFeaturePredictions.csv"
  }
  write.csv(gbm_predict, file = filename)
  return(gbm_predict)
}