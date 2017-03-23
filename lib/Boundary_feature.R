
#############################################################
### Construct features out of images for training/testing ###
#############################################################

# CONSTRUCTS BOUNDARY FEATURE

setwd("~/Desktop/sem 2/Applied data science/spr2017-proj3-group-12/data")

source("http://bioconductor.org/biocLite.R")
biocLite("EBImage")
library(EBImage)

bin_dir <- "~/Desktop/sem 2/Applied data science/Proj3/training_data/bin_image"

n_files <- length(list.files(bin_dir))

feature <- function(bin_dir) {
  
  ### Constructs features out of images for training/testing
  
  ### bin_dir: class "character", path to directory of new images

  
  file_names <- list.files(bin_dir, pattern = "[[:digit:]]_bin.jpg")
  file_paths <- rep(NA_character_, length(file_names))
  for (i in 1:length(file_names)) {
    file_paths[i] <- paste(bin_dir, file_names[i], sep = "/")
  }
  file_paths <- sort(file_paths)
  
  location_feature <- matrix(NA, nrow = 64*64, ncol = n_files)

  for (i in 1:n_files) {
    tryCatch({
      img_bw <- readImage(file_paths[i])
      img_bw <- thresh(img_01, w=50, h=50, offset=0.05)
      
      oc2 <- ocontour(bwlabel(img_02))
      b2 <- do.call(rbind.data.frame, oc2)
      n_x <- 16
      n_y <- 16
      x_Bin <- seq(0, 256, length.out=n_x)
      y_Bin <- seq(0, 256, length.out=n_y)
      freq_location <- as.data.frame(table(factor(findInterval(b2[,1], x_Bin), levels=1:n_x),
                                           factor(findInterval(b2[,2], x_Bin), levels=1:n_y)))
      location_feature[,i] <- as.numeric(freq_location$Freq)/(ncol(b2)*nrow(b2))
    }, 
    error = function(c) "invalid or corrupt JPEG file")
  }
  return(gray_feature)
}

location_feature <- feature(bin_dir)
write.csv(location_feature, file = "location.csv")


system.time(feature(bin_dir))
