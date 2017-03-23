###########################################
### Construting Train and test features ###
###########################################
### Author: Vikas Arun
### Modifier: Boxuan Zhao
### Project 3
### ADS spring 2017

dataSplit = function(percentage = 0.25, test = F)#Test indicate do we run on the new test data provided in class(Default : No)
{
  #Note: Non-useful ouputs are all muted to save time
  source("../lib/feature.R")
  library("EBImage")#Rememner to run this package before you do anything!!!!!!!!!
  if(!test)
  {
    sift.ori = read.csv("../data/train_data/sift_features.csv")
  
    gray_feature <- feature.gray()
  
    write.csv(gray_feature, file = "../data/train_data/gray.csv")
  
    gray = read.csv("../data/train_data/gray.csv")
  
  }
  else if(test)
  {
    sift.ori = read.csv("../data/test_data/sift_features.csv")
    
    gray_feature <- feature.gray("../data/test_data/raw_images")
    
    write.csv(gray_feature, file = "../data/test_data/gray.csv")
    
    gray = read.csv("../data/test_data/gray.csv")
  }
  
  #image_features_new = read.csv("../data/sift.feature.New.csv")
  sift.simp = feature.new(sift.ori)
  
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
  
  if(!test)
  {labels = read.csv("../data/train_data/labels.csv")}
  
  n = ncol(sift.ori)
  
  test_rows = sample(c(1:n), percentage*n, replace = FALSE)
  #train_rows = -test_rows
  
  #Output original sift features into test and train set
  #write.csv(sift.ori[,-test_rows], file = "../data/sift_ori_train.csv", row.names = FALSE)
  #write.csv(sift.ori[,test_rows], file = "../data/sift_ori_test.csv", row.names = FALSE)
  
  #Output Simplified sift features into test and train set
  #write.csv(sift.simp[,-test_rows], file = "../data/sift_simp_train.csv", row.names = FALSE)
  #write.csv(sift.simp[,test_rows], file = "../data/sift_simp_test.csv", row.names = FALSE)
 
  if(!test) 
  {#Output simplified sift features with gray features into test and train set
  write.csv(sift.simp.gray[,-test_rows], file = "../data/train_data/sift_simp_gray_train.csv", row.names = FALSE)
  write.csv(sift.simp.gray[,test_rows], file = "../data/train_data/sift_simp_gray_test.csv", row.names = FALSE)
  
  #Output original sift features with gray features into test and train set
  #write.csv(sift.ori.gray[,-test_rows], file = "../data/sift_ori_gray_train.csv", row.names = FALSE)
  #write.csv(sift.ori.gray[,test_rows], file = "../data/sift_ori_gray_test.csv", row.names = FALSE)
  
  #output class labels in to test and train set
  write.csv(labels[-test_rows,], file = "../data/train_data/labels_train.csv", row.names = FALSE)
  write.csv(labels[test_rows,], file = "../data/train_data/labels_test.csv", row.names = FALSE)}
  else if(test)
  {
    write.csv(sift.simp.gray, file = "../data/test_data/sift_simp_gray.csv", row.names = FALSE)
    
  }
  
  
}


