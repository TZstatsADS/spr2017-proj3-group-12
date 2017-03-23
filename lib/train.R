#########################################################
### Train a classification model with training images ###
#########################################################

### Author: Vikas Arun
### Project 3
### ADS Spring 2016

source("GBM.R")
source("RandomForest.R")

train = function(feature_filename, labels_filename, full_feature = FALSE, run_cv = FALSE, run_OOB = FALSE, K = 5)
{
  if(run_cv == TRUE)
  {
    ExploreGBM(feature_filename, labels_filename, K)
  }
  if(run_OOB == TRUE)
  {
    RandomForestExploration()
  }
  gbm_model = TrainGBM(feature_filename, labels_filename, full_feature = full_feature)
  rf_model = trainRandomForest(feature_filename, labels_filename, full_feature = full_feature)
  return(list(gbm_model, rf_model))
}  

