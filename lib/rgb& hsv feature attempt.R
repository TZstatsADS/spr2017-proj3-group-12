#########RGB & HSV features #######################################


img_dir <- "~/GitHub/spr2017-proj3-group-12/training_data/training_data/raw_images/"

library("EBImage")
file_names <- list.files(img_dir)
n_files <- length(list.files(img_dir))

########### RGB features prep ##############
nR <- 10
nG <- 8
nB <- 10 
rBin <- seq(0, 1, length.out=nR)
gBin <- seq(0, 1, length.out=nG)
bBin <- seq(0, 1, length.out=nB)
mat=array()
mat_as_rgb=array()
freq_rgb=array()
rgb_feature=matrix(nrow=20, ncol=nR*nG*nB)

########### HSV features prep ##############
library(grDevices)
nH <- 10
nS <- 6
nV <- 6
hBin <- seq(0, 1, length.out=nH)
sBin <- seq(0, 1, length.out=nS)
vBin <- seq(0, 0.005, length.out=nV) 
hsv_feature <- matrix(nrow=20, ncol=nH*nS*nV)

############# Real business starts here #######################
###### Extract 800 RGB & 360 HSV features for chicken #########

for (i in 1:1000){
  mat <- imageData(readImage(paste0(img_dir, "image", "_", sprintf("%04.f",i), ".jpg")))
  mat_as_rgb <-array(c(mat,mat,mat),dim = c(nrow(mat),ncol(mat),3))
  freq_rgb <- as.data.frame(table(factor(findInterval(mat_as_rgb[,,1], rBin), levels=1:nR), factor(findInterval(mat_as_rgb[,,2], gBin), levels=1:nG), factor(findInterval(mat_as_rgb[,,3], bBin), levels=1:nB)))
  rgb_feature[i,] <- as.numeric(freq_rgb$Freq)/(ncol(mat)*nrow(mat)) # normalization
  
  mat_rgb <-mat_as_rgb
  dim(mat_rgb) <- c(nrow(mat_as_rgb)*ncol(mat_as_rgb), 3)
  mat_hsv <- rgb2hsv(t(mat_rgb))
  freq_hsv <- as.data.frame(table(factor(findInterval(mat_hsv[1,], hBin), levels=1:nH), factor(findInterval(mat_hsv[2,], sBin), levels=1:nS),  factor(findInterval(mat_hsv[3,], vBin), levels=1:nV)))
  hsv_feature[i,] <- as.numeric(freq_hsv$Freq)/(ncol(mat_as_rgb)*nrow(mat_as_rgb)) # normalization
}

rgb_chicken <- rgb_feature
hsv_chicken <- hsv_feature

###### Extract 800 RGB & 360 HSV features for dogs #########
rgb_dog=matrix(nrow=1000, ncol=nR*nG*nB)
hsv_dog <-matrix(nrow=1000, ncol=nH*nS*nV)

for (i in 1001:2000){
  mat <- imageData(readImage(paste0(img_dir,"image", "_", i, ".jpg")))
  mat_as_rgb <-array(c(mat,mat,mat),dim = c(nrow(mat),ncol(mat),3))
  freq_rgb <- as.data.frame(table(factor(findInterval(mat_as_rgb[,,1], rBin), levels=1:nR), factor(findInterval(mat_as_rgb[,,2], gBin), levels=1:nG), factor(findInterval(mat_as_rgb[,,3], bBin), levels=1:nB)))
  rgb_dog[i-1000,] <- as.numeric(freq_rgb$Freq)/(ncol(mat_as_rgb)*nrow(mat_as_rgb)) # normalization
  
  mat_rgb <- mat_as_rgb
  dim(mat_rgb) <- c(nrow(mat_as_rgb)*ncol(mat_as_rgb), 3)
  mat_hsv <- rgb2hsv(t(mat_rgb))
  freq_hsv <- as.data.frame(table(factor(findInterval(mat_hsv[1,], hBin), levels=1:nH), factor(findInterval(mat_hsv[2,], sBin), levels=1:nS),  factor(findInterval(mat_hsv[3,], vBin), levels=1:nV)))
  hsv_dog[i-1000,] <- as.numeric(freq_hsv$Freq)/(ncol(mat_as_rgb)*nrow(mat_as_rgb)) # normalization
}  

### combine RGB and HSV features of chicken and dog 
rgb_feature=rbind(rgb_chicken,rgb_dog)
hsv_feature=rbind(hsv_chicken,hsv_dog)

### add feature names
colnames(rgb_feature) <- paste0("RGB",1:800)
colnames(hsv_feature) <- paste0("HSV",1:360)

### add response label to the last column
label <- c(rep(1,1000),rep(0,1000))
rgb_feature=cbind(rgb_feature,label)
hsv_feature=cbind(hsv_feature,label)


save(rgb_feature, file="~/GitHub/spr2017-proj3-group-12/output/rgb_feature_train.RData")
save(hsv_feature, file="~/GitHub/spr2017-proj3-group-12/output/hsv_feature_train.RData")