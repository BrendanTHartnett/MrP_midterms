library(randomForest)
library(caret)
library(keras)
library(tensorflow)
require(caTools)

data <- read_csv("model_prediction_classifier_dat.csv")
names <- c(1:9)
data[,names] <- lapply(data[,names] , factor)

sample = sample.split(data$biden_winner, SplitRatio = .75)
train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)
dim(train)
dim(test)

rf <- NA
rf <- randomForest(biden_winner~., data=train, proximity=TRUE)
print(rf)

pred =  predict(rf, newdata=test[-1])

test_l = as.factor(test$biden_winner)


confusionMatrix(test_l, pred)
