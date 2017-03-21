#setwd("C:/Users/Vikas/OneDrive/Cloud Workspace/Documents/Columbia/Senior/Applied Data Science/Project 3/spr2017-proj3-group-12/lib")

dataSplit = function(percentage = 0.25)
{
  source("./lib/feature.R")
  
  image_features = read.csv("./data/sift_features.csv")
  
  #image_features_new = read.csv("../data/sift.feature.New.csv")
  image_features_new = feature.new(image_features)
  
  image_features_gray = read.csv("./data/gray.csv")
  
  #Remove the first column of observation numbers
  image_features_gray = image_features_gray[,2:2001]
  
  #Remove fetures that have no variation (if any)
  image_features_gray = image_features_gray[which(apply(image_features_gray,1,sd)!=0),]
  
  #Rename the columns
  colnames(image_features_gray) = colnames(image_features_new)
  
  image_features_new_gray = rbind(image_features_new,image_features_gray)
  
  image_labels = read.csv("./data/labels.csv")
  
  n = ncol(image_features)
  
  test_rows = sample(c(1:n), percentage*n, replace = FALSE)
  #train_rows = -test_rows
  write.csv(image_features[,-test_rows], file = "./data/sift_features_train.csv", row.names = FALSE)
  write.csv(image_features[,test_rows], file = "./data/sift_features_test.csv", row.names = FALSE)
  write.csv(image_labels[-test_rows,], file = "./data/labels_train.csv", row.names = FALSE)
  write.csv(image_labels[test_rows,], file = "./data/labels_test.csv", row.names = FALSE)
  write.csv(image_features_new[,-test_rows], file = "./data/sift_features_new_train.csv", row.names = FALSE)
  write.csv(image_features_new[,test_rows], file = "./data/sift_features_new_test.csv", row.names = FALSE)
  write.csv(image_features_new_gray[,-test_rows], file = "./data/sift_new_gray_train.csv", row.names = FALSE)
  write.csv(image_features_new_gray[,test_rows], file = "./data/sift_new_gray_test.csv", row.names = FALSE)
}
