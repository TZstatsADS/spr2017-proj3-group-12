
#############################################################
### Construct features out of images for training/testing ###
#############################################################

# CONSTRUCTS HARMONIC COEFFICIENTS FROM ELLIPTICAL FOURIER OUTLINE ANALYSIS

setwd("~/Desktop/sem 2/Applied data science/spr2017-proj3-group-12/data")

source("http://bioconductor.org/biocLite.R")
biocLite("EBImage")
library(EBImage)
install.packages("devtools")
devtools::install_github("vbonhomme/Momocs")
library(Momocs)

img_dir <- "~/Desktop/sem 2/Applied data science/Proj3/training_data/raw_images"
bin_dir <- "~/Desktop/sem 2/Applied data science/Proj3/training_data/bin_image"
data_dir <- "~/Desktop/sem 2/Applied data science/spr2017-proj3-group-12/output/features_z"

n_files <- length(list.files(img_dir))
#dat2 <- array(dim=c(n_files,1))

feature <- function(img_dir, data_dir) {
  
  ### Constructs features out of images for training/testing
  
  ### img_dir: class "character", path to directory of images to be processed
  ### data_dir: class "character", path to directory to place feature data files in
  ### Output: .rds files, one for each image, containing the features for that image
  
  ##### CURRENT STATUS (2016/03/18 19:30): 
  ##### This function constructs only harmonic coefficient features.
  ##### WARNING: This function also writes a new processed image file per image.
  #####          This will thus double the number of images in your image directory.
  ##### Maybe a separate directory for the processed files should be created.
  ##### Running time on Arnold's computer:
  ##### user: 2655.92 system: 43.69 elapsed 2824.62 (approx 47 minutes)
  
  file_names <- list.files(img_dir, pattern = "[[:digit:]].jpg") # THIS IS NOT A GOOD SOLUTION
  file_names <- sort(file_names)
  file_paths <- rep(NA_character_, length(file_names))
  for (i in 1:length(file_names)) {
    file_paths[i] <- paste(img_dir, file_names[i], sep = "/")
  }
  file_paths <- sort(file_paths)
#  file_names2 <- list.files(bin_dir, pattern = "[[:digit:]]_bin.jpg") # THIS IS NOT A GOOD SOLUTION
#  file_names2 <- sort(file_names2)
#  file_paths2 <- rep(NA_character_, length(file_names2))
#  for (i in 1:length(file_names2)) {
#    file_paths2[i] <- paste(bin_dir, file_names2[i], sep = "/")
#  }
#  file_paths2 <- sort(file_paths2)
  
  gray_feature <- matrix(NA, nrow = 256, ncol = n_files)
#  location_feature <- matrix(NA, nrow = 64*64, ncol = n_files)
  
  # Construct harmonic coefficient features from image outline
  # Note: Some images may be invalid; features will not be constructed for those images
  # Note: Some images may result in outlines with too little detail; features will not be
  #       constructed for those images
  for (i in 1:n_files) {
    tryCatch({
      img_gray <- readImage(file_paths[i])
#      img_bw <- readImage(file_paths2[i])
#      img_gray <- resize(img_gray, 64, 64)
#      img_bw <- resize(img_bw, 64, 64)
      mat <- imageData(img_gray)
      n <- 256
      nBin <- seq(0, 1, length.out = n)
      freq_gray <- as.data.frame(table(factor(findInterval(mat, nBin), levels = 1:n)))
      gray_feature[,i] <- as.numeric(freq_gray$Freq)/(ncol(mat)*nrow(mat))
#      gray_feature[,i] <- as.vector(mat)
      
#      img_bw <- thresh(img_01, w=50, h=50, offset=0.05)

#      oc2 <- ocontour(bwlabel(img_02))
#      b2 <- do.call(rbind.data.frame, oc2)
#      n_x <- 16
#      n_y <- 16
#      x_Bin <- seq(0, 256, length.out=n_x)
#      y_Bin <- seq(0, 256, length.out=n_y)
#      freq_location <- as.data.frame(table(factor(findInterval(b2[,1], x_Bin), levels=1:n_x),
#                                           factor(findInterval(b2[,2], x_Bin), levels=1:n_y)))
#      location_feature[,i] <- as.numeric(freq_location$Freq)/(ncol(b2)*nrow(b2))
      
    }, 
    error = function(c) "invalid or corrupt JPEG file")
  }
  return(gray_feature)
#  write.csv(location_feature, file = "location.csv")
}

gray_feature <- feature(img_dir, data_dir)
write.csv(gray_feature, file = "gray.csv")


system.time(feature(img_dir, data_dir))
