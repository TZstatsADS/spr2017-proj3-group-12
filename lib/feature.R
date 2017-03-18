#############################################################
### Construct visual features for training/testing images ###
#############################################################

### Authors: Yuting Ma/Tian Zheng
### Project 3
### ADS Spring 2017

feature <- function(img_dir, set_name, data_name="data", export=T){
  
  ### Construct process features for training/testing images
  ### Sample simple feature: Extract row average raw pixel values as features
  
  ### Input: a directory that contains images ready for processing
  ### Output: an .RData file contains processed features for the images
  
  ### load libraries
  library("EBImage")
  
  n_files <- length(list.files(img_dir))
  
  ### determine img dimensions
  img0 <-  readImage(paste0(img_dir, "img", "_", data_name, "_", set_name, "_", 1, ".jpg"))
  mat1 <- as.matrix(img0)
  n_r <- nrow(img0)
  
  ### store vectorized pixel values of images
  dat <- matrix(NA, n_files, n_r) 
  for(i in 1:n_files){
    img <- readImage(paste0(img_dir,  "img", "_", data_name, "_", set_name, "_", i, ".jpg"))
    dat[i,] <- rowMeans(img)
  }
  
  ### output constructed features
  if(export){
    save(dat, file=paste0("../output/feature_", data_name, "_", set_name, ".RData"))
  }
  return(dat)
}

#feature.new = function()
#{
 # y <- read.csv("D:/Columbia University/Spring2017-Applied Data Science/Project_3_Bz2290/spr2017-proj3-group-12/data/labels.csv", header = T)
  #oriData <- read.csv("D:/Columbia University/Spring2017-Applied Data Science/Project_3_Bz2290/spr2017-proj3-group-12/data/sift_features.csv", header = T)
  #X <- oriData
  #n_feature <- nrow(X)
  #n_case <- ncol(X)
#}
