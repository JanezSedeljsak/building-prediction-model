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

# Evaluacija atribuiov
#install.packages('Boruta')
#library(Boruta)
#
#boruta_output <- Boruta(namembnost ~ ., data=na.omit(train), doTrace=0)
#boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
#print(boruta_signif)
#
#roughFixMod <- TentativeRoughFix(boruta_output)
#boruta_signif <- getSelectedAttributes(roughFixMod)
#print(boruta_signif)
#
#imps <- attStats(roughFixMod)
#imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
#head(imps2[order(-imps2$meanImp), ])  # descending sort
#
#plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")
##povrsina + leto_izgradnje + stavba + tedenska_poraba + poraba + vcerajsnja_poraba + regija + temp_zraka = povrsina + leto_izgradnje + stavba + tedenska_poraba + poraba + vcerajsnja_poraba + regija + temp_zraka
## best 8 ()
#
set.seed(0)
library(CORElearn)

# tree mdl true (0.4884197)
# bayers mdl true (0.4448579)
# rf mdl true (0.56000334)

# Glasovanje
modelDT <- CoreModel(namembnost ~ ., train, model="tree", selectionEstimator="MDL", binaryEvaluation=T)
modelKNN <- CoreModel(namembnost ~ ., train, model="knn", kInNN = 5)
modelRF <- CoreModel(namembnost ~ ., train, model="rf", selectionEstimator="MDL", binaryEvaluation=T)
modelNB <- CoreModel(namembnost ~ ., train, model="bayes", selectionEstimator="MDL")
#modelKNN6 <- CoreModel(namembnost ~ ., train, model="knn", kInNN = 6)
#modelKNN4 <- CoreModel(namembnost ~ ., train, model="knn", kInNN = 4)
#modelRT <- CoreModel(namembnost ~ ., train, model="regTree")
#modelRFN <- CoreModel(namembnost ~ ., train, model="rfNear")

predNB <- predict(modelNB, test, type="class")
caNB <- CA(test$namembnost, predNB)
caNB # 0.4448579

predDT <- predict(modelDT, test, type="class")
caDT <- CA(test$namembnost, predDT)
caDT # 0.4884197

predRF <- predict(modelRF, test, type="class")
caRF <- CA(test$namembnost, predRF)
caRF # 0.5585702

predKNN <- predict(modelKNN, test, type="class")
caKNN <- CA(test$namembnost, predKNN)
caKNN # 0.5756271

# predikcije modelov
pred <- data.frame(predRF, predKNN, predDT)
head(pred)

predNamembnost <- voting(pred)
predicted <- factor(predNamembnost, levels=levels(train$namembnost))
CA(test$namembnost, predicted) # 0.5980769

#for (mn in c("tree", "bayes", "rf", "tree")) {
#    for (se in c("MDL", "InfGain", "Gini", "GainRatio", "ReliefFequalK")) {
#        for (be in c(T, F)) {
#            print(paste(mn, paste(se, be)))
#            model <- CoreModel(namembnost ~ ., train, model=mn, selectionEstimator=se, binaryEvaluation=be) # 0.45
#            pred <- predict(model, test, type="class")
#            ca <- CA(test$namembnost, pred)
#            print(ca)
#        }
#    }
#}

# Utezeno glasovanje
predDT.prob <- predict(modelDT, test, type="prob")
predRF.prob <- predict(modelRF, test, type="prob")
predKNN.prob <- predict(modelKNN, test, type="prob")
predNB.prob <- predict(modelNB, test, type="prob")

pred.prob <- predRF.prob + predKNN.prob
predNamembnost <- colnames(pred.prob)[max.col(pred.prob)]
predicted.prob <- factor(predNamembnost, levels(train$namembnost))
CA(test$namembnost, predicted.prob) 
# 0.5900084 (predRF.prob * caRF + predKNN.prob * caKNN * 2)
# 0.5723662 (predRF.prob + predKNN.prob)
# tree MDL TRUE

#library(ipred)
#
#mymodel <- function(formula, data, target.model){CoreModel(formula, data, model=target.model)}
#mypredict <- function(object, newdata) {pred <- predict(object, newdata, type="class"); destroyModels(object); pred}
#
#res <- errorest(namembnost ~ ., train, model=mymodel, predict=mypredict, target.model="tree")
#caDT.cv <- 1 - res$error
#caDT.cv
#
#res <- errorest(namembnost ~ ., train, model=mymodel, predict=mypredict, target.model="rf")
#caRF.cv <- 1 - res$error
#caRF.cv
#
#mymodelKNN <- function(formula, data, valK){CoreModel(formula, data, model="knn", kInNN=valK)}
#res <- errorest(namembnost ~ ., train, model=mymodelKNN, predict=mypredict, valK=5)
#caKNN.cv <- 1 - res$error
#caKNN.cv
#
## sedaj pri sestevanju napovedane verjetnosti utezimo s pricakovano tocnostjo modela
#predProb <- caDT.cv * predDT.prob + caRF.cv * predRF.prob + caKNN.cv * predKNN.prob
#predNamembnost <- colnames(predProb)[max.col(predProb)]
#predicted <- factor(predNamembnost, levels(train$namembnost))
#
#CA(test$namembnost, predicted) # 0.5114967

# Bagging
library(ipred)

bag <- bagging(namembnost ~ ., train, nbagg=30)
predicted <- predict(bag, test, type="class")
CA(test$namembnost, predicted) # 0.5044526

# Nakljucni gozd
library(randomForest)

rf_train <- na.omit(train)
rf_test <- na.omit(test)

rf <- randomForest(namembnost ~ ., rf_train)
predicted <- predict(rf, rf_test, type="class")
CA(rf_test$namembnost, predicted) # 0.536648

# Boosting
library(adabag)

bm <- boosting(namembnost ~ ., train, mfinal=100)
predictions <- predict(bm, test)
names(predictions)

predicted <- predictions$class
CA(test$namembnost, predicted) # 0.5125
