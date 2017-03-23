#############################################################
### Construct visual features for training/testing images ###
#############################################################

### Authors: Boxuan Zhao(feature.new)
###          Boya Zhao(feature.gray)
###          Vikas Arun(DataSplit)
### Project 3
### ADS Spring 2017

#######################################
### Construct enhanced sift feature ###
#######################################

feature.new = function(dat)
{
  variation = apply(dat,1,sd) 
  
  #Thereshold value for known varaince
  thereshold = summary(variation)[2]
  
  dat = dat[which(variation > thereshold),]
  
  data.dog = dat[,1001:2000]
  data.chicken = dat[,1:1000]
  
  avg.dog = apply(data.dog,1,mean)
  avg.chicken = apply(data.chicken,1,mean)
  
  avg.difference = abs(avg.dog - avg.chicken)
  
  large = which(avg.difference > summary(avg.difference)[3])
  
  dat = dat[large,]
  
  return(dat)
}


#############################################################
### Construct features out of images for training/testing ###
#############################################################

# CONSTRUCTS GRAYSCALE FEATURE


#source("http://bioconductor.org/biocLite.R")
#biocLite("EBImage")
#library(EBImage)

feature <- function(dir) {
  
  ### Constructs features out of images for training/testing
  
  ### img_dir: class "character", path to directory of images to be processed
  
  ##### CURRENT STATUS (2016/03/18 19:30): 
  ##### This function constructs only grayscale features.
  
  #source("http://bioconductor.org/biocLite.R")
  #biocLite("EBImage")
  #library(EBImage)
  img_dir = paste(dir,"raw_images",sep="")
  n_files <- length(list.files(img_dir))
  file_names <- list.files(img_dir, pattern = "[[:digit:]].jpg")
  file_names <- sort(file_names)
  file_paths <- rep(NA_character_, length(file_names))
  for (i in 1:length(file_names)) {
    file_paths[i] <- paste(img_dir, file_names[i], sep = "/")
  }
  file_paths <- sort(file_paths)
  
  gray_feature <- matrix(NA, nrow = 256, ncol = n_files)
  
  #constructed for those images
  for (i in 1:n_files) {
    tryCatch({
      img_gray <- readImage(file_paths[i])
      mat <- imageData(img_gray)
      n <- 256
      nBin <- seq(0, 1, length.out = n)
      freq_gray <- as.data.frame(table(factor(findInterval(mat, nBin), levels = 1:n)))
      gray_feature[,i] <- as.numeric(freq_gray$Freq)/(ncol(mat)*nrow(mat))
    }, 
    error = function(c) "invalid or corrupt JPEG file")
  }
  return(gray_feature)
}



###########################################
### Construting Train and test features ###
###########################################
### Author: Vikas Arun
### Modifier: Boxuan Zhao

dir <- "../data/train_data/"
#feature(img_train_dir,"train",export=TRUE)
dataSplit = function(dir, percentage = 0.25, export = F)
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
  
  tm_train = NA
  tm_test = NA
  
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
  
  labels = read.csv(paste(dir,"labels.csv",sep=""))
  n = ncol(sift.ori)
  
  test_rows = sample(c(1:n), percentage*n, replace = FALSE)
  
  tm_train = system.time({
  
  sift.simp.gray.train = sift.simp.gray[,-test_rows]})
  tm_test = system.time({
  sift.simp.gray.test = sift.simp.gray[,test_rows]})
  
  labels.train = labels[-test_rows,]
  
  labels.test = labels[test_rows,]
  
  write.csv(sift.simp.gray.train, file = paste(dir,"sift_simp_gray_train.csv",sep=""), row.names = FALSE)
  write.csv(sift.simp.gray.test, file = paste(dir,"sift_simp_gray_test.csv",sep=""), row.names = FALSE)
  write.csv(labels.train, file = paste(dir,"labels_train.csv",sep=""), row.names = FALSE)
  write.csv(labels.test, file = paste(dir,"labels_test.csv",sep=""), row.names = FALSE)
    
  return(list(train=tm_train,test=tm_test))
}



##############################################3
source("http://bioconductor.org/biocLite.R")
biocLite("EBImage")
library(EBImage)


setwd("~/Desktop/sem 2/Applied data science/spr2017-proj3-group-12/data")
img_dir <- "~/Desktop/sem 2/Applied data science/Proj3/training_data/raw_images"

feature <- function(img_dir="../train_data/raw_images") {
  
  ### Constructs features out of images for training/testing
  
  ### img_dir: class "character", path to directory of images to be processed
  
  ##### CURRENT STATUS (2016/03/18 19:30): 
  ##### This function constructs only grayscale features.
  
  n_files <- length(list.files(img_dir))
  file_names <- list.files(img_dir, pattern = "[[:digit:]].jpg")
  file_names <- sort(file_names)
  file_paths <- rep(NA_character_, length(file_names))
  for (i in 1:length(file_names)) {
    file_paths[i] <- paste(img_dir, file_names[i], sep = "/")
  }
  file_paths <- sort(file_paths)
  
  gray_feature <- matrix(NA, nrow = 256, ncol = n_files)
  
  #       constructed for those images
  for (i in 1:n_files) {
    tryCatch({
      img_gray <- readImage(file_paths[i])
      mat <- imageData(img_gray)
      n <- 256
      nBin <- seq(0, 1, length.out = n)
      freq_gray <- as.data.frame(table(factor(findInterval(mat, nBin), levels = 1:n)))
      gray_feature[,i] <- as.numeric(freq_gray$Freq)/(ncol(mat)*nrow(mat))
    }, 
    error = function(c) "invalid or corrupt JPEG file")
  }
  return(gray_feature)
}

gray_feature <- feature(img_dir)
write.csv(gray_feature, file = "gray.csv")

system.time(feature(img_dir))