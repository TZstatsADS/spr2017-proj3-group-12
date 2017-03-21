rawsift <- read.csv("sift_features/sift_features.csv", fill=TRUE, header = TRUE, stringsAsFactors = FALSE)
rawsift <- t(rawsift)
rawsift <- as.data.frame(rawsift)
labelsift <- read.csv("labels.csv")

maxs <- apply(rawsift[,], 2, max)
mins <- apply(rawsift[,], 2, min)
scaled.data <- as.data.frame(scale(rawsift[,],center = mins, scale = maxs - mins))
print(head(scaled.data,2))
data = cbind(labelsift,scaled.data)
names(data)[1] <- "classified"

library(caTools)
set.seed(101)
split = sample.split(data$classified, SplitRatio = 0.75)
train = subset(data, split == TRUE)
test = subset(data, split == FALSE)

feats <- names(scaled.data)
f <- paste(feats,collapse=' + ')
f <- paste('classified ~',f)
f <- as.formula(f)

install.packages("neuralnet")
library(neuralnet)
nn <- neuralnet(f,train,hidden=c(20,20,20),linear.output=FALSE)

predicted.nn.values <- compute(nn,test[2:5001])
print(head(predicted.nn.values$net.result))
predicted.nn.values$net.result <- sapply(predicted.nn.values$net.result,round,digits=0)
table(test$classified,predicted.nn.values$net.result)
