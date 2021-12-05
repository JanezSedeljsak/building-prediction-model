setwd("C:\\Users\\Marko\\Documents\\GitHub\\ai-heating-consumption")
setwd("D:\\ai-heating-consumption")

# pogosto uporabljene funkcije
# 
#install.packages("caret")
library(caret)
library(nnet)
source("common.r") 

train <- readWithFactorize("ucna.txt")
test <- readWithFactorize("test.txt")
namembnostMatrix <- class.ind(test$namembnost)

##############################
#        Klasifikacija       #
##############################

# Trivialen model
getTrivialCA <- function(curTrain, curTest) {
    set.seed(0)
    tab <- table(curTrain$namembnost)
    max_namembnost <- names(tab)[which.max(tab)]
    predTrivial <- rep(max_namembnost, nrow(curTest))
    namembnostModelStats(curTest$namembnost, predTrivial, T) # 0.47023
}

getTrivialCA(train, test)

library(CORElearn)
library(ipred)
library(randomForest)
library(adabag)

source("wrapper.r")
set.seed(0)

# Ocenjevanje atributov nad drevesom in izrisi
sort(attrEval(namembnost ~ ., train, "InfGain"), decreasing = T)
sort(attrEval(namembnost ~ ., train, "Gini"), decreasing = T)
sort(attrEval(namembnost ~ ., train, "MDL"), decreasing = T)
sort(attrEval(namembnost ~ ., train, "ReliefFequalK"), decreasing = T)

# InfGain top 3
dt1 <- CoreModel(namembnost ~ povrsina + regija + stavba, train, model="tree")

# Gini top 4
dt2 <- CoreModel(namembnost ~ povrsina + leto_izgradnje + regija + stavba, train, model="tree")

# Mdl top 5
dt3 <- CoreModel(namembnost ~ povrsina + regija + stavba + leto_izgradnje + temp_zraka, train, model="tree")

# ReliefFequalK top 6
dt4 <- CoreModel(namembnost ~ leto_izgradnje + stavba + povrsina + poraba + regija + tedenska_poraba, train, model="tree")

######################
# Graf najboljših atributov
evalING <- attrEval(namembnost ~ ., test, "InfGain")
evalGIN <- attrEval(namembnost ~ ., test, "Gini")
evalMDL <- attrEval(namembnost ~ ., test, "MDL")
evalREK <- attrEval(namembnost ~ ., test, "ReliefFequalK")


library(RColorBrewer)
coul <- brewer.pal(4, "Set3")
allEvals <- rbind(evalING, evalGIN, evalMDL, evalREK)
barplot(allEvals, beside=T, cex.names=0.7, col=coul)
legend("top", c("InfGain", "Gini", "MDL", "ReliefFequalK"), cex=0.9, fill=coul)


predDT1 <- predict(dt1, test, type="class")
namembnostModelStats(test$namembnost, predNB, T) # 0.445150
plot(dt1, train, type=5)

predDT2 <- predict(dt2, test, type="class")
namembnostModelStats(test$namembnost, predNB, T) # 0.4451505
plot(dt2, train, type=5)

predDT3 <- predict(dt3, test, type="class")
namembnostModelStats(test$namembnost, predNB, T) # 0.445150
plot(dt3, train, type=5)

predDT4 <- predict(dt4, test, type="class")
namembnostModelStats(test$namembnost, predNB, T) # 0.445150
plot(dt4, train, type=5)

# Wrapper ni izboljšal predikcije
#wrapper(namembnost ~ ., train, myTrainFunc, myPredictFunc, myEvalFunc, cvfolds=10)

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
namembnostModelStats(test$namembnost, predNB, T) # 0.4448578

predDT <- predict(modelDT, test, type="class") # Odlocitveno drevo
namembnostModelStats(test$namembnost, predDT, T) # 0.488419

predBM <- predict(modelBM, test)$class # Boosting
namembnostModelStats(test$namembnost, predBM, T) # 0.501546

predRF2 <- predict(modelRF2, na.omit(test), type="class") # Nakljucni gozd (implementacija 2) - brez NA vrednosti
namembnostModelStats(na.omit(test)$namembnost, predRF2, T) # 0.542527685

predBAG <- predict(modelBAG, test)$class # Bagging
namembnostModelStats(test$namembnost, predBAG, T) # 0.5200668

predRFN <- predict(modelRFN, test, type="class") # Nakljucni gozd z utezenostjo
namembnostModelStats(test$namembnost, predRFN, T) # 0.5605769230

predKNNK <- predict(modelKNNK, test, type="class") # K najblizjih sosedov z utezmi
namembnostModelStats(test$namembnost, predKNNK, T) # 0.562249163

predRF <- predict(modelRF, test, type="class") # Nakljucni gozd (implementacija 1)
namembnostModelStats(test$namembnost, predRF, T) # 0.55974080267

predKNN <- predict(modelKNN, test, type="class") # K najblizjih sosedov
namembnostModelStats(test$namembnost, predKNN, T) # 0.57830267

# Glasovanje
pred <- data.frame(predRFN, predKNN, predDT, predBM)
head(pred)

predNamembnost <- voting(pred)
predicted <- as.factor(predNamembnost)
CA(test$namembnost, predicted) # 0.59765886

# Utezeno glasovanje
predDT.prob <- predict(modelDT, test, type="prob")
predRF.prob <- predict(modelRF, test, type="prob")
predRFN.prob <- predict(modelRFN, test, type="prob")
predKNN.prob <- predict(modelKNN, test, type="prob")
predBM.prob <- predict(modelBM, test)[[3]]
predBAG.prob <- predict(modelBAG, test)[[3]]
predKNNK.prob <- predict(modelKNNK, test, type="prob")

print(paste("Brier DT:  ", paste(" ", brier.score(namembnostMatrix, predDT.prob)))) # 1.023160535117
print(paste("Brier RF:  ", paste(" ", brier.score(namembnostMatrix, predRF.prob)))) # 0.6553150753013
print(paste("Brier KNN: ", paste(" ", brier.score(namembnostMatrix, predKNN.prob)))) # 0.657498327759
print(paste("Brier BAG: ", paste(" ", brier.score(namembnostMatrix, predBAG.prob)))) # 0.892684824

pred.prob <- predRF.prob + predKNN.prob + predKNNK.prob 
predNamembnost <- colnames(pred.prob)[max.col(pred.prob)]
predicted.prob <- factor(predNamembnost, NAMEMBNOST_LEVELS)
namembnostModelStats(test$namembnost, predicted.prob, T) # 0.58641304

addAccuracy <- function(actual, prob, pred) {
    prob * CA(actual, pred)
}

pred.prob <- addAccuracy(test$namembnost, predRF.prob, predRF) + addAccuracy(test$namembnost, predKNN.prob, predKNN) + 
    addAccuracy(test$namembnost, predKNNK.prob, predKNNK)

predNamembnost <- colnames(pred.prob)[max.col(pred.prob)]
predicted.prob <- factor(predNamembnost, NAMEMBNOST_LEVELS)
namembnostModelStats(test$namembnost, predicted.prob, T) # 0.586872909

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

bayesPredictions <- c()
rfPredictions <- c()

for (m in seq(2, 12, 1)) {
    curModelRF <- CoreModel(namembnost ~ ., tmpTrain, model="rf")
    curModelBayes <- CoreModel(namembnost ~ ., tmpTrain, model="bayes")

    curPredRF <- predict(curModelRF, tmpTest, type="class")
    curPredBayes <- predict(curModelBayes, tmpTest, type="class")

    bayesPredictions[m] <- CA(tmpTest$namembnost, curPredBayes)
    rfPredictions[m] <- CA(tmpTest$namembnost, curPredRF)

    print(paste("Testiranje v mesecu: ", m))
    print(paste("Bayes:          ", bayesPredictions[m]))
    print(paste("Naključni gozd: ", rfPredictions[m]))

    tmpTrain <- rbind(tmpTrain, tmpTest)
    tmpTest <- allData[allData$mesec == m + 1, ]
}

plot(x=1:12, y=rfPredictions, type="l", lty=1, ylim=c(0,1), 
    bty="n", main="Napredek učenja z postopnim dodajanjem",
    xlab="Mesec", ylab="Klasifikacijska natančnost", col="blue",
    sub="Rdeča - Bayes, Modra - Naključni gozd")

lines(x=1:12, y=bayesPredictions, lty=1, col="red")


