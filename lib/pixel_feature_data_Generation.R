#Generate the test and train data set for pixel features
list.of.packages <- c("EBImage", "jpeg")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(EBImage) # not available (for R version 3.3.1)
library(jpeg)


#Set up direction
dir = "D:/Columbia University/Spring2017-Applied Data Science/Project_3_Bz2290/Resized pictures/"

#reference the pixel feature extraction function
source("../spr2017-proj3-group-12/lib/pixel_feature_Extraction_function.R")

#Seperate 25% of the data as the testing data set
test.index = as.character(sample(1:2000,2000*0.25,replace = FALSE))


train.index = as.character(which(!((1:2000) %in% test.index)))


#Generate the test and train data set for the sift feature
#feature <- function(img_dir, set_name, export=T,test.index,train.index)
Data = feature(img_dir= dir,test.index = test.index, train.index = train.index)


#Store the train pictures into the pixel_train folder



#Store the test picures into the pixel_test folder



dat_train <- feature(img_train_dir, "img_zip_train")
data_test <- feature(img_test+dir, "img_zip_train")
save(dat_train, file="./output/feature_train.RData")
save(dat_test, file="./output/feature_test.RData")