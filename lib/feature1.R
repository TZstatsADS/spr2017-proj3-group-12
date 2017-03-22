
#############################################################
### Construct features out of images for training/testing ###
#############################################################

# CONSTRUCTS HARMONIC COEFFICIENTS FROM ELLIPTICAL FOURIER OUTLINE ANALYSIS

setwd("~/Desktop/sem 2/Applied data science/spr2017-proj3-group-12/data")

source("http://bioconductor.org/biocLite.R")
biocLite("EBImage")
library(EBImage)

img_dir <- "~/Desktop/sem 2/Applied data science/Proj3/training_data/raw_images"

n_files <- length(list.files(img_dir))

feature <- function(img_dir) {
  
  ### Constructs features out of images for training/testing
  
  ### img_dir: class "character", path to directory of images to be processed
  
  ##### CURRENT STATUS (2016/03/18 19:30): 
  ##### This function constructs only harmonic coefficient features.
  ##### WARNING: This function also writes a new processed image file per image.
  #####          This will thus double the number of images in your image directory.
  ##### Maybe a separate directory for the processed files should be created.
  ##### Running time on Arnold's computer:
  ##### user: 2655.92 system: 43.69 elapsed 2824.62 (approx 47 minutes)
  
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
