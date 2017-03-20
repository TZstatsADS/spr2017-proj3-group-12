install.packages("class")
library(class)

install.packages("gmodels")
library(gmodels)

rawsift <- read.csv("sift_features/sift_features.csv", fill=TRUE, header = TRUE, stringsAsFactors = FALSE)
label <- read.csv("labels.csv")
rawsift <- t(rawsift)
rawsiftdf <- as.data.frame(rawsift)
sift <- cbind(label, rawsiftdf)
names(sift)[1] <- "classified"

table(sift$classified)

sift_train_0 <- sift[1:750,]
sift_train_1 <- sift[1001:1750,]
sift_test_0 <- sift[751:1000,]
sift_test_1 <- sift[1751:2000,]
sift_train <- rbind(sift_train_0, sift_train_1)
sift_train <- sift_train[-1]
sift_test <- rbind(sift_test_0, sift_test_1)
sift_test <- sift_test[-1]

sift_train_labels <- sift_train[, 1]
sift_test_labels <- sift_test[, 1]

sift_test_pred <- knn(train = sift_train, test = sift_test, cl = sift_train_labels, k=5)
CrossTable(x = sift_test_labels, y = sift_test_pred, prop.chisq = FALSE )
