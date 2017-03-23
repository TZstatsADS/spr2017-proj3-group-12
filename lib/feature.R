#############################################################
### Construct visual features for training/testing images ###
#############################################################

### Project 3
### ADS Spring 2017


#############################################################
### Construct features out of images for training/testing ###
#############################################################

# CONSTRUCTS GRAYSCALE FEATURE with Sift feature


feature <- function(img_dir="../data/training_data/raw_images") {
  
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
  
  sift.simp = read.csv("../data/training_data/sift_features.csv")
  
  variation = apply(sift.simp,1,sd) 
  
  #Thereshold value for known varaince
  thereshold = summary(variation)[2]
  
  sift.simp = sift.simp[which(variation > thereshold),]
  
  data.dog = sift.simp[,1001:2000]
  data.chicken = sift.simp[,1:1000]
  
  avg.dog = apply(data.dog,1,mean)
  avg.chicken = apply(data.chicken,1,mean)
  
  avg.difference = abs(avg.dog - avg.chicken)
  
  large = which(avg.difference > summary(avg.difference)[3])
  
  sift.simp = sift.simp[large,]
  
  #Remove fetures that have no variation (if any)
  gray_feature = gray_feature[which(apply(gray_feature,1,sd)!=0),]
  
  #Rename the columns
  colnames(gray_feature) = colnames(sift.simp)
  
  #Construct New feature set
  
  #Simplified Sift Features with gray features
  sift.simp.gray = as.matrix(rbind(sift.simp,gray_feature))
  
  #Write the feature as a csv file into the path
  write.csv(sift.simp.gray, file = "../data/sift_gray_all.csv", row.names = FALSE)
  
  return(sift.simp.gray)
}



#######################################
### Construct enhanced sift feature ###
#######################################

#feature.new = function(dat)
#{
 # variation = apply(dat,1,sd) 
  
  #Thereshold value for known varaince
  #thereshold = summary(variation)[2]
  
  #dat = dat[which(variation > thereshold),]
  
  #data.dog = dat[,1001:2000]
  #data.chicken = dat[,1:1000]
  
  #avg.dog = apply(data.dog,1,mean)
  #avg.chicken = apply(data.chicken,1,mean)
  
  #avg.difference = abs(avg.dog - avg.chicken)
  
  #large = which(avg.difference > summary(avg.difference)[3])
  
#  dat = dat[large,]
  
 # return(dat)
#}






