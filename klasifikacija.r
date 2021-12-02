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
getTrivialCA <- function(curTrain, curTest) {
    tab <- table(curTrain$namembnost)
    max_namembnost <- names(tab)[which.max(tab)]
    predTrivial <- rep(max_namembnost, nrow(curTest))
    namembnostModelStats(curTest$namembnost, predTrivial, T) # 0.47023
}

getTrivialCA(train, test)

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

#####################################################
# Učenje na ločenih regijah

trainVzhod <- factorize(train[train$regija == "vzhodna",])
trainZahod <- factorize(train[train$regija == "zahodna",])

testVzhod <- factorize(test[test$regija == "vzhodna",])
testZahod <- factorize(test[test$regija == "zahodna",])

# Vzhod
getTrivialCA(trainVzhod, testVzhod) # 0.51827182
vModelBM <- boosting(namembnost ~ ., trainVzhod, mfinal=100)
vModelKNN <- CoreModel(namembnost ~ ., trainVzhod, model="knn", kInNN = 5)
vModelRF <- CoreModel(namembnost ~ ., trainVzhod, model="rf", selectionEstimator="MDL", binaryEvaluation=T)

vPredBM <- predict(vModelBM, testVzhod)$class # Boosting
namembnostModelStats(testVzhod$namembnost, vPredBM, T) # 0.7220522

vPredRF <- predict(vModelRF, testVzhod, type="class") # Nakljucni gozd (implementacija 1)
namembnostModelStats(testVzhod$namembnost, vPredRF, T) # 0.756795

vPredKNN <- predict(vModelKNN, testVzhod, type="class") # K najblizjih sosedov
namembnostModelStats(testVzhod$namembnost, vPredKNN, T) # 0.6462646


# Zahod
getTrivialCA(trainZahod, testZahod) # 0.42857142
zModelBM <- boosting(namembnost ~ ., trainZahod, mfinal=100)
zModelKNN <- CoreModel(namembnost ~ ., trainZahod, model="knn", kInNN = 5)
zModelRF <- CoreModel(namembnost ~ ., trainZahod, model="rf", selectionEstimator="MDL", binaryEvaluation=T)

zPredBM <- predict(zModelBM, testZahod)$class # Boosting
namembnostModelStats(testZahod$namembnost, zPredBM, T) # 0.2395784

zPredRF <- predict(zModelRF, testZahod, type="class") # Nakljucni gozd (implementacija 1)
namembnostModelStats(testZahod$namembnost, zPredRF, T) # 0.3925839

zPredKNN <- predict(zModelKNN, testZahod, type="class") # K najblizjih sosedov
namembnostModelStats(testZahod$namembnost, zPredKNN, T) # 0.4451990

#########################################
# Razdelitev po mesecih

allData <- rbind(train, test)
tmpTrain <- allData[allData$mesec == 1, ]
tmpTest <- allData[allData$mesec == 2, ]


for (m in seq(2, 12, 1)) {
    curModel <- CoreModel(namembnost ~ ., tmpTrain, model="rf", selectionEstimator="MDL")
    curPred <- predict(curModel, tmpTest, type="class")
    print(paste(paste("Testiranje v mesecu: ", m), paste(" ", CA(tmpTest$namembnost, curPred))))
    tmpTrain <- rbind(tmpTrain, tmpTest)
    tmpTest <- allData[allData$mesec == m + 1, ]
}
