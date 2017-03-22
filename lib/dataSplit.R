###########################################
### Construting Train and test features ###
###########################################
### Author: Vikas Arun
### Modifier: Boxuan Zhao
### Project 3
### ADS spring 2017

dataSplit = function(percentage = 1)
{
  #Note: Non-useful ouputs are all muted to save time
  source("../lib/feature.R")
  
  sift.ori = read.csv("../data/sift_features.csv")
  
  #image_features_new = read.csv("../data/sift.feature.New.csv")
  sift.simp = feature.new(sift.ori)
  
  gray_feature <- feature.gray()
  
  write.csv(gray_feature, file = "../data/gray.testing.csv")
  
  gray = read.csv("../data/gray.testing.csv")
  
  #Remove the first column of observation numbers
  gray = gray[,2:2001]
  
  #Remove fetures that have no variation (if any)
  gray = gray[which(apply(gray,1,sd)!=0),]
  
  #Rename the columns
  colnames(gray) = colnames(sift.simp)
  
  #Construct New feature set
  
  #Simplified Sift Features with gray features
  sift.simp.gray = rbind(sift.simp,gray)
  
  #Original Sift features with gray features
  sift.ori.gray = rbind(sift.ori,gray)
    
  labels = read.csv("../data/labels.csv")
  
  n = ncol(sift.ori)
  
  test_rows = sample(c(1:n), percentage*n, replace = FALSE)
  #train_rows = -test_rows
  
  #Output original sift features into test and train set
  #write.csv(sift.ori[,-test_rows], file = "../data/sift_ori_train.csv", row.names = FALSE)
  #write.csv(sift.ori[,test_rows], file = "../data/sift_ori_test.csv", row.names = FALSE)
  
  #Output Simplified sift features into test and train set
  #write.csv(sift.simp[,-test_rows], file = "../data/sift_simp_train.csv", row.names = FALSE)
  #write.csv(sift.simp[,test_rows], file = "../data/sift_simp_test.csv", row.names = FALSE)
  
  #Output simplified sift features with gray features into test and train set
  write.csv(sift.simp.gray[,-test_rows], file = "../data/sift_simp_gray_train.csv", row.names = FALSE)
  write.csv(sift.simp.gray[,test_rows], file = "../data/sift_simp_gray_test.csv", row.names = FALSE)
  
  #Output original sift features with gray features into test and train set
  #write.csv(sift.ori.gray[,-test_rows], file = "../data/sift_ori_gray_train.csv", row.names = FALSE)
  #write.csv(sift.ori.gray[,test_rows], file = "../data/sift_ori_gray_test.csv", row.names = FALSE)
  
  #output class labels in to test and train set
  write.csv(labels[-test_rows,], file = "../data/labels_train.csv", row.names = FALSE)
  write.csv(labels[test_rows,], file = "../data/labels_test.csv", row.names = FALSE)
  
  
  
}
