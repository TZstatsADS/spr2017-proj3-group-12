
#############################################################
### Construct features out of images for training/testing ###
#############################################################

# CONSTRUCTS GRAYSCALE FEATURE


source("http://bioconductor.org/biocLite.R")
biocLite("EBImage")
library(EBImage)


setwd("~/Desktop/sem 2/Applied data science/spr2017-proj3-group-12/data")
img_dir <- "~/Desktop/sem 2/Applied data science/Proj3/training_data/raw_images"

n_files <- length(list.files(img_dir))

feature <- function(img_dir) {
  
  ### Constructs features out of images for training/testing
  
  ### img_dir: class "character", path to directory of images to be processed
  
  ##### CURRENT STATUS (2016/03/18 19:30): 
  ##### This function constructs only grayscale features.
  
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
