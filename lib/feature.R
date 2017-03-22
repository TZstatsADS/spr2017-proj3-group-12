#############################################################
### Construct visual features for training/testing images ###
#############################################################

### Authors: Yuting Ma/Tian Zheng(feature)
###          Boxuan Zhao(feature.new)
###          Boya Zhao(feature.gray)
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

feature.gray <- function(img_dir = "../data/train_data/raw_images") {
  
  ### Constructs features out of images for training/testing
  
  ### img_dir: class "character", path to directory of images to be processed
  
  ##### CURRENT STATUS (2016/03/18 19:30): 
  ##### This function constructs only grayscale features.
  
  #source("http://bioconductor.org/biocLite.R")
  #biocLite("EBImage")
  #library(EBImage)
  
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

#gray_feature <- feature(img_dir)
#write.csv(gray_feature, file = "gray.csv")

#system.time(feature(img_dir))

