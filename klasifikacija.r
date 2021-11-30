setwd("C:\\Users\\Marko\\Documents\\GitHub\\ai-heating-consumption")
setwd("D:\\ai-heating-consumption")

# pogosto uporabljene funkcije
source("common.r") 

train <- readWithFactorize("ucna.txt")
test <- readWithFactorize("test.txt")

##############################
#        Klasifikacija       #
##############################

# Trivialen model
max_namembnost <- sort(table(train$namembnost), decreasing=T)[[1]]
predTrivial <- rep(max_namembnost, nrow(test))
caTrivial <- CA(test$namembnost, predTrivial)
caTrivial # 0.47

set.seed(0)
library(CORElearn)

# Glasovanje
modelDT <- CoreModel(namembnost ~ ., train, model="tree") # 0.45
modelKNN <- CoreModel(namembnost ~ ., train, model="knn", kInNN = 5) # 0.56
modelRF <- CoreModel(namembnost ~ ., train, model="rf")
#modelNB <- CoreModel(namembnost ~ ., train, model="bayes") # 0.35
#modelKNN6 <- CoreModel(namembnost ~ ., train, model="knn", kInNN = 6)
#modelKNN4 <- CoreModel(namembnost ~ ., train, model="knn", kInNN = 4)
#modelRT <- CoreModel(namembnost ~ ., train, model="regTree")
#modelRFN <- CoreModel(namembnost ~ ., train, model="rfNear")

predDT <- predict(modelDT, test, type = "class")
caDT <- CA(test$namembnost, predDT)
caDT # 0.4702341

predRF <- predict(modelRF, test, type="class")
caRF <- CA(test$namembnost, predRF)
caRF # 0.5521739

predKNN <- predict(modelKNN, test, type="class")
caKNN <- CA(test$namembnost, predKNN)
caKNN # 0.5756271

# predikcije modelov
pred <- data.frame(predRF, predKNN, predDT)
head(pred)

predNamembnost <- voting(pred)
predicted <- factor(predNamembnost, levels=levels(train$namembnost))
CA(test$namembnost, predicted) # 0.5920987
