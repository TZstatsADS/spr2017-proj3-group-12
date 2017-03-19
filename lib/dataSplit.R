setwd("C:/Users/Vikas/OneDrive/Cloud Workspace/Documents/Columbia/Senior/Applied Data Science/Project 3/spr2017-proj3-group-12/lib")

dataSplit = function()
{
  image_features = read.csv("../data/sift_features.csv")
  image_features_new = read.csv("../data/sift.feature.New.csv")
  image_labels = (read.csv("../data/labels.csv"))
  n = ncol(image_features)
  test_rows = sample(c(1:n), .25*n, replace = FALSE)
  train_rows = -test_rows
  write.csv(image_features[,train_rows], file = "../data/sift_features_train.csv", row.names = FALSE)
  write.csv(image_features[,test_rows], file = "../data/sift_features_test.csv", row.names = FALSE)
  write.csv(image_labels[train_rows,], file = "../data/labels_train.csv", row.names = FALSE)
  write.csv(image_labels[test_rows,], file = "../data/labels_test.csv", row.names = FALSE)
  write.csv(image_features_new[,train_rows], file = "../data/sift_features_new_train.csv", row.names = FALSE)
  write.csv(image_features_new[,test_rows], file = "../data/sift_features_new_test.csv", row.names = FALSE)
}