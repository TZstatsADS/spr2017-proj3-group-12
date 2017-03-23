dataSplit.Inclass = function(dir="../data/test_data/", percentage = 0.25)
{
  ### Constructs testing and training features out of images for training/testing
  
  ### dir: direction to the data set where images and other data files are stored
  ### percentage: a numeric vairable to indeicate how much will be regarded as test data(default set to be 25%)
  ### InclassData: a boolean variable indeicate wheter to split training data(given before the due date) or construct feature from testing data(given in the class at due day).
  ###          default:split the train data according to the percentage specified by user
  ### export: Whether to export data
  #Note: Non-useful ouputs are all muted to save time
  
  #source("../lib/feature.R")
  library("EBImage")#Rememner to run this package before you do anything!!!!!!!!!
  
  sift.ori = read.csv(paste(dir,"sift_features.csv",sep=""))
  
  gray_feature <- feature.gray(dir)
  
  sift.simp = feature.new(sift.ori)
  
  #Remove fetures that have no variation (if any)
  gray_feature = gray_feature[which(apply(gray_feature,1,sd)!=0),]
  
  #Rename the columns
  colnames(gray_feature) = colnames(sift.simp)
  
  #Construct New feature set
  
  #Simplified Sift Features with gray features
  sift.simp.gray = rbind(sift.simp,gray_feature)
  
  write.csv(sift.simp.gray, file = paste(dir,"sift_simp_gray.csv",sep=""), row.names = FALSE)
  
  
}



