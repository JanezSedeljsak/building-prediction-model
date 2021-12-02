setwd("C:\\Users\\Marko\\Documents\\GitHub\\ai-heating-consumption")
setwd("D:\\ai-heating-consumption")

# pogosto uporabljene funkcije
# 
#install.packages("caret")
library(caret)
source("common.r") 

train <- readWithFactorize("ucna.txt")
test <- readWithFactorize("test.txt")

##############################
#        Klasifikacija       #
##############################

# Trivialen model
set.seed(0)
tab <- table(train$namembnost)
max_namembnost <- names(tab)[which.max(tab)]
predTrivial <- rep(max_namembnost, nrow(test))
namembnostModelStats(test$namembnost, predTrivial, T) # 0.47023

library(CORElearn)
library(ipred)
library(randomForest)
library(adabag)

set.seed(0)
modelDT <- CoreModel(namembnost ~ ., train, model="tree", selectionEstimator="MDL", binaryEvaluation=T)
modelKNN <- CoreModel(namembnost ~ ., train, model="knn", kInNN = 5)
modelRF <- CoreModel(namembnost ~ ., train, model="rf", selectionEstimator="MDL", binaryEvaluation=T)
modelNB <- CoreModel(namembnost ~ ., train, model="bayes", selectionEstimator="MDL")
modelRFN <- CoreModel(namembnost ~ ., train, model="rfNear", selectionEstimator="MDL", binaryEvaluation=T)
modelKNNK <- CoreModel(namembnost ~ ., train, model="knnKernel", kInNN = 5)
modelBAG <- bagging(namembnost ~ ., train, nbagg=20)
modelRF2 <- randomForest(namembnost ~ ., na.omit(train))
modelBM <- boosting(namembnost ~ ., train, mfinal=100)
#######################################

# Evaluacije zgornjih modelov
predNB <- predict(modelNB, test, type="class") # Naivni Bayes
namembnostModelStats(test$namembnost, predNB, T) # 0.4448579

predDT <- predict(modelDT, test, type="class") # Odlocitveno drevo
namembnostModelStats(test$namembnost, predDT, T) # 0.4884197

predBM <- predict(modelBM, test)$class # Boosting
namembnostModelStats(test$namembnost, predBM, T) # 0.50618

predRF2 <- predict(modelRF2, na.omit(test), type="class") # Nakljucni gozd (implementacija 2) - brez NA vrednosti
namembnostModelStats(na.omit(test)$namembnost, predRF2, T) # 0.5418426761

predBAG <- predict(modelBAG, test, type="class") # Bagging
namembnostModelStats(test$namembnost, predBAG, T) # 0.5328177s

predRFN <- predict(modelRFN, test, type="class") # Nakljucni gozd z utezenostjo
namembnostModelStats(test$namembnost, predRFN, T) # 0.5585284

predKNNK <- predict(modelKNNK, test, type="class") # K najblizjih sosedov z utezmi
namembnostModelStats(test$namembnost, predKNNK, T) # 0.55928093

predRF <- predict(modelRF, test, type="class") # Nakljucni gozd (implementacija 1)
namembnostModelStats(test$namembnost, predRF, T) # 0.5585702

predKNN <- predict(modelKNN, test, type="class") # K najblizjih sosedov
namembnostModelStats(test$namembnost, predKNN, T) # 0.5756271


# Glasovanje
pred <- data.frame(predRF, predKNN, predictedBM, predDT)
head(pred)

predNamembnost <- voting(pred)
predicted <- as.factor(predNamembnost)
namembnostModelStats(test$namembnost, predicted, T) # 0.5980769230769

# Utezeno glasovanje
predDT.prob <- predict(modelDT, test, type="prob")
predRF.prob <- predict(modelRF, test, type="prob")
predKNN.prob <- predict(modelKNN, test, type="prob")
predBM.prob <- predict(modelBM, test)[[3]]
predKNNK.prob <- predict(modelKNNK, test, type="prob")

pred.prob <- predRF.prob + predKNN.prob
predNamembnost <- colnames(pred.prob)[max.col(pred.prob)]
predicted.prob <- factor(predNamembnost, NAMEMBNOST_LEVELS)
namembnostModelStats(test$namembnost, predicted.prob, T)
# 0.5862876 (predRF.prob * 0.558 + predKNN.prob + predKNNK.prob * 0.56)
# 0.5900084 (predRF.prob * caRF + predKNN.prob * caKNN * 2)
# 0.5725753 (predRF.prob + predKNN.prob)
