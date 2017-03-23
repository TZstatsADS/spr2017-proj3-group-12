feature.gray <- function(img_dir="../data/test_data/raw_images") {
    
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

dataSplit.Inclass = function(dir="../data/test_data/", percentage = 0.25)
{
  ### Constructs testing and training features out of images for training/testing
  
  ### dir: direction to the data set where images and other data files are stored
  ### percentage: a numeric vairable to indeicate how much will be regarded as test data(default set to be 25%)
  ### InclassData: a boolean variable indeicate wheter to split training data(given before the due date) or construct feature from testing data(given in the class at due day).
  ###          default:split the train data according to the percentage specified by user
  ### export: Whether to export data
  #Note: Non-useful ouputs are all muted to save time
  
  source("../lib/feature.R")
  library("EBImage")#Rememner to run this package before you do anything!!!!!!!!!
  
  sift.ori = read.csv(paste(dir,"sift_features.csv",sep=""))
  
  gray_feature <- feature.gray()
  
  sift.simp = feature.new(sift.ori)
  
  #Remove fetures that have no variation (if any)
  gray_feature = gray_feature[which(apply(gray_feature,1,sd)!=0),]
  
  #Rename the columns
  colnames(gray_feature) = colnames(sift.simp)
  
  #Construct New feature set
  
  #Simplified Sift Features with gray features
  sift.simp.gray = rbind(sift.simp,gray_feature)
  
  write.csv(sift.simp.gray, file = paste(dir,"sift_simp_gray.csv",sep=""), row.names = FALSE)
  
  
}



