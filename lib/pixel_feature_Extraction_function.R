#############################################################
### Construct visual features for training/testing images ###
#############################################################

### Original Authors: Yuting Ma/Tian Zheng
### Modifier : Boxuan Zhao
### Project 3
### ADS Spring 2017

feature <- function(img_dir,export=T,test.index,train.index){
  
  ### Construct process features for training/testing images

  ### Input: a directory that contains images ready for processing
  ### Output: an .RData file contains processed features for the images
  ###         a list contains the train data set and test data set
  ### load libraries
  library("EBImage")
  
  n_files <- length(list.files(img_dir))
  
  ### determine img dimensions
  img0 <-  readImage(paste0(img_dir, "image", "_","000",1,".jpg",sep=""))#Good
  mat1 <- as.matrix(img0)
  n_r <- nrow(img0)
  
  ### store vectorized pixel values of images
  dat.test <- matrix(NA, length(test.index), n_r)
  number.0 = 4 -  nchar(test.index)
  for(i in 1:length(test.index)){
    img <- readImage(paste0(img_dir,"image_",as.character(rep(0,number.0[i])),test.index[i],".jpg",sep=""))
    dat.test[i,] <- rowMeans(img)
  }
  
  dat.train <- matrix(NA, length(train.index), n_r)
  number.0 = 4 -  nchar(train.index)
    for(i in 1:length(train.index)){
    img <- readImage(paste0(img_dir,"image_",as.character(rep(0,number.0[i])),train.index[i],".jpg",sep=""))
    dat.train[i,] <- rowMeans(img)
  }
  ### output constructed features
  if(export){
    save(dat.test, file=paste0("../output/feature_","pixel_test",".RData",sep=""))
    save(data.train,file=paste0("../output/feature_","pixel_train",".RData",sep=""))
  }
  return(list(dat.test,dat.train))
}
