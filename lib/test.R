######################################################
### Fit the classification model with testing data ###
######################################################

### Author: Vikas Arun
### Project 3
### ADS Spring 2016

source("../lib/GBM.R")
source("../lib/RandomForest.R")

test_models = function(gbm_model, rf_model, feature_filename, full_feature = FALSE)
{
  gbm_predict = TestGBM(gbm_model, feature_filename, full_feature = full_feature)
  rf_predict = testRandomForest(rf_model, feature_filename, full_feature)
  return(list(gbm_predict, rf_predict))
}
