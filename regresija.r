##############################
#         Regresija          #
##############################

setwd("C:\\Users\\Marko\\Documents\\GitHub\\ai-heating-consumption")
setwd("D:\\ai-heating-consumption")

# pogosto uporabljene funkcije
source("common.r")

train <- readWithFactorize("ucna.txt")
test <- readWithFactorize("test.txt")

train <- na.omit(train)
test <- na.omit(test)

# Observed podatki so vedno enaki
observed <- test$poraba

#########################
# OCENJEVANJE ATRIBUTOV #
#########################

# Uporabimo različne algoritme za evalvacijo atributov, da vidimo, kateri so najbolj uporabni za regresijo

library(CORElearn)

# vcerajsnja_poraba + tedenska_poraba + povrsina + leto_izgradnje + stavba + namembnost + regija
paste(names(sort(attrEval(poraba ~ ., train, "MSEofMean"), decreasing = TRUE)), collapse = ' + ')

# vcerajsnja_poraba + tedenska_poraba + povrsina + vikend + pritisk + padavine + regija
paste(names(sort(attrEval(poraba ~ ., train, "RReliefFequalK"), decreasing = TRUE)), collapse = ' + ')

# vcerajsnja_poraba + tedenska_poraba + povrsina + leto_izgradnje + stavba + namembnost + hitrost_vetra
paste(names(sort(attrEval(poraba ~ ., train, "RReliefFbestK"), decreasing = TRUE)), collapse = ' + ')

########

# vikend + vcerajsnja_poraba + temp_rosisca + temp_zraka + tedenska_poraba + oblacnost + smer_vetra
paste(names(sort(attrEval(poraba ~ ., train, "MSEofModel"), decreasing = TRUE)), collapse = ' + ')

# vcerajsnja_poraba + tedenska_poraba + povrsina + leto_izgradnje + stavba + namembnost + padavine
paste(names(sort(attrEval(poraba ~ ., train, "RReliefFsqrDistance"), decreasing = TRUE)), collapse = ' + ')


# Če povzamemo kateri atributi so dobri pri večini načinih evalvacije:
# vcerajsnja_poraba, tedenska_poraba, povrsina


#############################
# MODELIRANJE in EVALVACIJA #
#############################

#####################
# Linear model - lm #
#####################
set.seed(0)
model <- lm(poraba ~ ., train)
predicted <- predict(model, test)
rmse(observed, predicted, mean(train$poraba)) # 0.06519753
mae(observed, predicted)                      # 19.31359

# Dodajanje atributov, ki so se obnesli v ocenjevanju atributov:

set.seed(0)
model <- lm(poraba ~ vcerajsnja_poraba + tedenska_poraba + povrsina, train)
predicted <- predict(model, test)
rmse(observed, predicted, mean(train$poraba)) # 0.05745394
mae(observed, predicted)                      # 17.88321

set.seed(0)
model <- lm(poraba ~ vcerajsnja_poraba + tedenska_poraba, train)
predicted <- predict(model, test)
rmse(observed, predicted, mean(train$poraba)) # 0.05731692
mae(observed, predicted)                      # 17.54313

set.seed(0)
model <- lm(poraba ~ vcerajsnja_poraba, train)
predicted <- predict(model, test)
rmse(observed, predicted, mean(train$poraba)) # 0.072729
mae(observed, predicted)                      # 17.85158

set.seed(0)
model <- lm(poraba ~ vcerajsnja_poraba + tedenska_poraba + vikend, train)
predicted <- predict(model, test)
rmse(observed, predicted, mean(train$poraba)) # 0.0541383
mae(observed, predicted)                      # 19.04113


# Uporabimo lahko wrapper, da ugotovimo kateri parametri se najbolje obnesejo
source("wrapper.R")

# Funkcija za ucenje modela
myTrainFuncReg <- function(formula, traindata)
{
  lm(formula, traindata)
}


# Funkcija za pridobivanje napovedi modela
myPredictFuncReg <- function(model, testdata)
{
  predict(model, testdata)
}


# Funkcija za ocenjevanje kvalitete modela (v tem primeru RMSE)
myEvalFuncRMSE <- function(predicted, observed, trained)
{
  rmse(observed, predicted, mean(trained))
}


set.seed(0)
wrapper(poraba ~ ., train, myTrainFuncReg, myPredictFuncReg, myEvalFuncRMSE, cvfolds=100)


set.seed(0)
model <- lm(poraba ~ vcerajsnja_poraba + tedenska_poraba + vikend + povrsina + oblacnost + padavine + temp_rosisca + leto_izgradnje + sezona + temp_zraka, train)
predicted <- predict(model, test)
rmse(observed, predicted, mean(train$poraba)) # 0.05426703
mae(observed, predicted)                      # 19.21306




#############################
# Regresijsko drevo - rpart #
#############################
library(rpart)
library(rpart.plot)

set.seed(0)
model <- rpart(poraba ~ ., train)
predicted <- predict(model, test)
rmse(observed, predicted, mean(train$poraba))  # 0.1288535
mae(observed, predicted)                       # 44.48812
rpart.plot(model)

set.seed(0)
model <- rpart(poraba ~ vcerajsnja_poraba + tedenska_poraba, train)
predicted <- predict(model, test)
rmse(observed, predicted, mean(train$poraba))  # 0.1288535
mae(observed, predicted)                       # 44.48812
rpart.plot(model)


set.seed(0)
# najprej zgradimo veliko drevo (nastavitev cp=0)
rt.model <- rpart(poraba ~ ., data=train, cp=0)
#rpart.plot(rt.model)

# rpart med gradnjo drevesa interno ocenjuje njegovo kvaliteto 
tab <- printcp(rt.model)
tab

# izberemo vrednost parametra cp, ki ustreza minimalni napaki internega presnega preverjanja
row <- which.min(tab[,"xerror"])
th <- mean(c(tab[row, "CP"], tab[row-1, "CP"]))
th

# porezemo drevo z izbrano nastavitvijo
rt.model <- prune(rt.model, cp=th)
rpart.plot(rt.model)

set.seed(0)
predicted <- predict(rt.model, test)
rmse(observed, predicted, mean(train$poraba))  # 0.06194545
mae(observed, predicted)                       # 19.69517



################################
# Support-vector machine - svm #
################################
library(e1071)

set.seed(0)
svm.model <- svm(poraba ~ ., train)
predicted <- predict(svm.model, test)
rmse(observed, predicted, mean(train$poraba))  # 0.05762129
mae(observed, predicted)                       # 21.38413

myTrainFuncReg <- function(formula, traindata)
{
  svm(formula, traindata)
}
wrapper(poraba ~ ., train, myTrainFuncReg, myPredictFuncReg, myEvalFuncRMSE, cvfolds=2)

set.seed(0)
svm.model <- svm(poraba ~ vcerajsnja_poraba + tedenska_poraba + vikend + regija, train)
predicted <- predict(svm.model, test)
rmse(observed, predicted, mean(train$poraba))  # 0.04510532
mae(observed, predicted)                       # 19.29338

set.seed(0)
svm.model <- svm(poraba ~ vcerajsnja_poraba + tedenska_poraba + vikend, train)
predicted <- predict(svm.model, test)
rmse(observed, predicted, mean(train$poraba))  # 0.04445059
mae(observed, predicted)                       # 19.76115

set.seed(0)
svm.model <- svm(poraba ~ vcerajsnja_poraba + tedenska_poraba, train)
predicted <- predict(svm.model, test)
rmse(observed, predicted, mean(train$poraba))  # 0.06234921
mae(observed, predicted)                       # 21.2505


##############################
# k-najbližjih sosedov - knn #
##############################
library(kknn)

set.seed(0)
model <- kknn(poraba ~ ., train, test, k = 5)
predicted <- fitted(model)
rmse(observed, predicted, mean(train$poraba))  # 0.164123
mae(observed, predicted)                       # 52.56553

set.seed(0)
knn.model <- kknn(poraba ~ vcerajsnja_poraba + tedenska_poraba + vikend, train, test, k = 5)
predicted <- fitted(knn.model)
rmse(observed, predicted, mean(train$poraba))  # 0.05526724
mae(observed, predicted)                       # 17.97103

set.seed(0)
knn.model <- kknn(poraba ~ vcerajsnja_poraba + tedenska_poraba + vikend + povrsina, train, test, k = 5)
predicted <- fitted(knn.model)
rmse(observed, predicted, mean(train$poraba))  # 0.06698666
mae(observed, predicted)                       # 23.69035

set.seed(0)
knn.model <- kknn(poraba ~ vcerajsnja_poraba + tedenska_poraba + vikend, train, test, k = 10)
predicted <- fitted(knn.model)
rmse(observed, predicted, mean(train$poraba))  # 0.04661027
mae(observed, predicted)                       # 16.65517

set.seed(0)
knn.model <- kknn(poraba ~ vcerajsnja_poraba + tedenska_poraba + vikend, train, test, k = 20)
predicted <- fitted(knn.model)
rmse(observed, predicted, mean(train$poraba))  # 0.0435993
mae(observed, predicted)                       # 15.86111

set.seed(0)
knn.model <- kknn(poraba ~ vcerajsnja_poraba + tedenska_poraba + vikend, train, test, k = 30)
predicted <- fitted(knn.model)
rmse(observed, predicted, mean(train$poraba))  # 0.04334001
mae(observed, predicted)                       # 15.65017





######################
# KOMBINIRANO UČENJE #
######################

#####################################
# Bagging z uporabo random forest-a #
#####################################
library(randomForest)

set.seed(0)
model <- randomForest(poraba ~ ., train, mtry=17)
predicted <- predict(model, test)
rmse(observed, predicted, mean(train$poraba))  # 0.05821984
mae(observed, predicted)                       # 21.67558


############################################################
# Boosting z Generalized Boosted Regression Modeling (GBM) #
############################################################

#install.packages("gbm")
library(gbm)
set.seed(0)
model <- gbm(poraba ~ ., train, distribution = "gaussian", n.trees = 5000, interaction.depth = 4, shrinkage = 0.01)
predicted <- predict(model, test)
rmse(observed, predicted, mean(train$poraba))  # 0.0598647
mae(observed, predicted)                       # 19.66564

set.seed(0)
model <- gbm(poraba ~ ., train, distribution = "gaussian", n.trees = 100, interaction.depth = 8, shrinkage = 0.1)
predicted <- predict(model, test)
rmse(observed, predicted, mean(train$poraba))  # 0.05337431
mae(observed, predicted)                       # 19.01418

set.seed(0)
model <- gbm(poraba ~ ., train, distribution = "gaussian", n.trees = 1000, interaction.depth = 8, shrinkage = 0.01)
predicted <- predict(model, test)
rmse(observed, predicted, mean(train$poraba))  # 0.05253253
mae(observed, predicted)                       # 18.09985

set.seed(0)
model <- gbm(poraba ~ ., train, distribution = "gaussian", n.trees = 6000, interaction.depth = 8, shrinkage = 0.001)
predicted <- predict(model, test)
rmse(observed, predicted, mean(train$poraba))  # 0.05222664
mae(observed, predicted)                       # 17.32126




#########################################################
# PRIMERJANJE USPEŠNOSTI GLEDE NA PODATKE IZ OBEH REGIJ #
#########################################################

# razdelimo podatke na eno n drugo regijo
trainVzhod <- train[train$regija == "vzhodna",]
trainZahod <- train[train$regija == "zahodna",]

trainVzhod <- levels()

testVzhod <- test[test$regija == "vzhodna",]
testZahod <- test[test$regija == "zahodna",]

observedVzhod <- testVzhod$poraba
observedZahod <- testZahod$poraba

######
# lm #
######

#vzhod
set.seed(0)
model <- lm(poraba ~ vcerajsnja_poraba + tedenska_poraba, trainVzhod)
predicted <- predict(model, testVzhod)
rmse(observedVzhod, predicted, mean(trainVzhod$poraba)) # 0.07113724
mae(observedVzhod, predicted)                           # 24.56143

#Zahod
set.seed(0)
model <- lm(poraba ~ vcerajsnja_poraba + tedenska_poraba, trainZahod)
predicted <- predict(model, testZahod)
rmse(observedZahod, predicted, mean(trainZahod$poraba)) # 0.02711596
mae(observedZahod, predicted)                           # 12.68869


#######
# svm #
#######

library(e1071)

#vzhod
set.seed(0)
svm.model <- svm(poraba ~ ., trainVzhod)
predicted <- predict(svm.model, testVzhod)
rmse(observedVzhod, predicted, mean(trainVzhod$poraba)) # 0.05861002
mae(observedVzhod, predicted)                           # 26.40473

#Zahod
set.seed(0)
svm.model <- svm(poraba ~ ., trainZahod)
predicted <- predict(model, testZahod)
rmse(observedZahod, predicted, mean(trainZahod$poraba)) # 0.02711596
mae(observedZahod, predicted)                           # 12.68869


#######
# knn #
#######
library(kknn)

#vzhod
set.seed(0)
knn.model <- kknn(poraba ~ vcerajsnja_poraba + tedenska_poraba + vikend, trainVzhod, testVzhod, k = 10)
predicted <- fitted(knn.model)
rmse(observedVzhod, predicted, mean(trainVzhod$poraba)) # 0.05317498
mae(observedVzhod, predicted)                           # 21.06857

#Zahod
set.seed(0)
knn.model <- kknn(poraba ~ vcerajsnja_poraba + tedenska_poraba + vikend, trainZahod, testZahod, k = 10)
predicted <- fitted(knn.model)
rmse(observedZahod, predicted, mean(trainZahod$poraba)) # 0.0300223
mae(observedZahod, predicted)                           # 13.8744


# Opazimo, da podatki, ki so v Zahodni množici, bolj prispevajo k natančnosti modela



#########################
# Razdelitev po mesecih #
#########################

allData <- rbind(train, test)
tmpTrain <- allData[allData$mesec == 1, ]
tmpTest <- allData[allData$mesec == 2, ]

lmPredictions <- c()
svmPredictions <- c()

for (m in seq(2, 12, 1)) {
  curModelLm <- lm(poraba ~ vcerajsnja_poraba + tedenska_poraba + vikend, tmpTrain)
  curModelSVM <- svm(poraba ~ vcerajsnja_poraba + tedenska_poraba + vikend, tmpTrain)
  
  curPredLm <- predict(curModelLm, tmpTest)
  curPredSVM <- predict(curModelSVM, tmpTest)
  
  lmPredictions[m] <- rmse(observed, curPredLm, mean(train$poraba))
  svmPredictions[m] <- rmse(observed, curPredSVM, mean(train$poraba))
  
  print(paste("Testiranje v mesecu: ", m))
  print(paste("Linear model:           ", lmPredictions[m]))
  print(paste("Support-vector machine: ", svmPredictions[m]))
  
  tmpTrain <- rbind(tmpTrain, tmpTest)
  tmpTest <- allData[allData$mesec == m + 1, ]
}

plot(x=1:12, y=svmPredictions, type="l", lty=1, ylim=c(0,4), 
     bty="n", main="Napredek učenja z postopnim dodajanjem",
     xlab="Mesec", ylab="RMSE (manj je bolje)", col="blue",
     sub="Rdeča - LM, Modra - SVM")

lines(x=1:12, y=lmPredictions, lty=1, col="red")


