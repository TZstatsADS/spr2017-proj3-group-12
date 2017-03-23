
#############################################################
### Construct features out of images for training/testing ###
#############################################################

# CONSTRUCTS NEW BLACK AND WHITE IMAGES

source("http://bioconductor.org/biocLite.R")
biocLite("EBImage")
library(EBImage)

img_dir <- "~/Desktop/sem 2/Applied data science/Proj3/training_data/raw_images"
bin_dir <- "~/Desktop/sem 2/Applied data science/Proj3/training_data/bin_image"

n_files <- length(list.files(img_dir))

NEW_IMAGE <- function(img_dir, bin_dir) {
  
  ### Constructs new image to get new features
  
  ### img_dir: class "character", path to directory of images to be processed
  ### bin_dir: class "character", path to directory of new images
  
  file_names <- list.files(img_dir, pattern = "[[:digit:]].jpg")
  file_names <- sort(file_names)
  file_paths <- rep(NA_character_, length(file_names))
  for (i in 1:length(file_names)) {
    file_paths[i] <- paste(img_dir, file_names[i], sep = "/")
  }
  file_paths <- sort(file_paths)

  for (i in 1:n_files) {
    tryCatch({
      img_bin <- readImage(file_paths[i])
      threshold <- otsu(img_bin)
      img_bin <- img_bin > threshold # create binary black/white image using Otsu threshold
      writeImage(img_bin, paste(bin_dir, "/", unlist(strsplit(file_names[i], split = "[.]"))[1], 
                                "_bin.jpg", sep = ""), type = "jpeg")
    }, 
    error = function(c) "invalid or corrupt JPEG file")
  }
  return(gray_feature)
}