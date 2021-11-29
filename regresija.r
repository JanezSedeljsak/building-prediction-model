##############################
#         Regresija          #
##############################

setwd("C:\\Users\\Marko\\Documents\\GitHub\\ai-heating-consumption")
train <- read.csv("ucna.txt", stringsAsFactors = T)
test <- read.csv("test.txt", stringsAsFactors = T)

factorize <- function (data) {
  data$namembnost <- factor(data$namembnost, levels=c("izobrazevalna", "javno_storitvena", "kulturno_razvedrilna", "poslovna", "stanovanjska"))
  data$season <- factor(data$season, levels=c("Winter", "Spring", "Summer", "Autumn"))
  data$regija <- as.factor(data$regija)
  data$oblacnost <- factor(data$oblacnost, levels=seq(0,10,1))
  data$is_weekend <- as.factor(data$is_weekend)
  data$prejsnjaPoraba[data$prejsnjaPoraba == -1] <- NA
  data
}

train <- factorize(train)
test <- factorize(test)

source("common.r")

#train <- na.omit(train)
#test <- na.omit(test)

summary(train)

hist(log1p(train$povrsina))
hist(train$temp_zraka)
hist(train$temp_rosisca)
hist(train$padavine)
hist(train$pritisk)
hist(log1p(train$hitrost_vetra))
hist(log1p(train$poraba))
hist(train$prejsnjaPoraba)

# Vsi atributi pred obdelavo
set.seed(0)
model <- lm(poraba ~ ., train)
predicted <- predict(model, test)
observed <- test$poraba
rmae(observed, predicted, mean(train$poraba)) # 0.1475865


# Izbrani atributi
library(CORElearn)

# prejsnjaPoraba, povrsina, leto_izgradnje, stavba, namembnost, regija, temp_zraka
#sort(attrEval(poraba ~ ., train, "MSEofMean"), decreasing = TRUE)

# prejsnjaPoraba, povrsina, leto_izgradnje, padavine, regija, is_weekend
#sort(attrEval(poraba ~ ., train, "RReliefFequalK"), decreasing = TRUE)

# stavba leto_izgradnje, smer_vetra, temp_zraka, temp_rosisca
#sort(attrEval(poraba ~ ., train, "ReliefFexpRank"), decreasing = TRUE)

# prejsnjaPoraba, povrsina, leto_izgradnje, stavba, namembnost, regija
#sort(attrEval(poraba ~ ., train, "RReliefFbestK"), decreasing = TRUE)

# is_weekend, temp_rosisca, prejsnjaPoraba, povrsina, temp_zraka, oblacnost
#sort(attrEval(poraba ~ ., train, "MSEofModel"), decreasing = TRUE)

# prejsnjaPoraba, povrsina, leto_izgradnje, stavba, namembnost, regija
#sort(attrEval(poraba ~ ., train, "RReliefFsqrDistance"), decreasing = TRUE)

set.seed(0)
model <- lm(poraba ~ prejsnjaPoraba + namembnost, train)
predicted <- predict(model, test)
rmse(observed, predicted, mean(train$poraba)) # 0.06935539

set.seed(0)
model <- lm(poraba ~ prejsnjaPoraba + leto_izgradnje, train)
predicted <- predict(model, test)
rmse(observed, predicted, mean(train$poraba)) # 0.06934524

set.seed(0)
model <- lm(poraba ~ is_weekend + prejsnjaPoraba, train)
predicted <- predict(model, test)
rmse(observed, predicted, mean(train$poraba)) # 0.06466684

set.seed(0)
model <- lm(poraba ~ is_weekend + temp_rosisca + prejsnjaPoraba, train)
predicted <- predict(model, test)
rmse(observed, predicted, mean(train$poraba)) # 0.06990003

set.seed(0)
model <- lm(poraba ~ prejsnjaPoraba + povrsina, train)
predicted <- predict(model, test)
rmse(observed, predicted, mean(train$poraba)) # 0.06990003

set.seed(0)
model <- lm(poraba ~ prejsnjaPoraba, train)
predicted <- predict(model, test)
rmse(observed, predicted, mean(train$poraba)) # 0.06934569

set.seed(0)
model <- lm(poraba ~ oblacnost + namembnost + season + prejsnjaPoraba, train)
predicted <- predict(model, test)
rmse(observed, predicted, mean(train$poraba)) # 0.06924975


# Uporaba wrapperja za ugotavljanje najboljših parametrov


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
  sum((observed - predicted)^2)/sum((observed - mean(trained))^2)	
}

# z uporabo teh parametrov

###########################
# lm
###########################
set.seed(0)
model <- lm(poraba ~ ., train)
predicted <- predict(model, test)
rmse(observed, predicted, mean(train$poraba)) # 0.06519753

set.seed(0)
wrapper(poraba ~ ., train, myTrainFuncReg, myPredictFuncReg, myEvalFuncRMSE, cvfolds=10)

set.seed(0)
model <- lm(poraba ~ prejsnjaPoraba + is_weekend + povrsina + smer_vetra + oblacnost + leto_izgradnje + padavine + temp_zraka + season + regija + hitrost_vetra, train)
predicted <- predict(model, test)
rmse(observed, predicted, mean(train$poraba)) # 0.06514779

#############################
# rpart - regresijsko drevo #
#############################
library(rpart)
library(rpart.plot)

set.seed(0)
model <- rpart(poraba ~ ., train)
predicted <- predict(model, test)
rmse(observed, predicted, mean(train$poraba))  # 0.1315064

myTrainFuncReg <- function(formula, traindata)
{
  rpart(formula, traindata)
}
wrapper(poraba ~ ., train, myTrainFuncReg, myPredictFuncReg, myEvalFuncRMSE, cvfolds=10)

set.seed(0)
model <- rpart(poraba ~ prejsnjaPoraba + regija + stavba + namembnost + povrsina + leto_izgradnje + temp_zraka + temp_rosisca + oblacnost, train)
predicted <- predict(model, test)
rmse(observed, predicted, mean(train$poraba))  # 0.1315064
rpart.plot(model)

set.seed(0)
# najprej zgradimo veliko drevo (nastavitev cp=0)
rt.model <- rpart(poraba ~ ., data=train, cp=0)
#rpart.plot(rt.model)

# rpart med gradnjo drevesa interno ocenjuje njegovo kvaliteto 
tab <- printcp(rt.model)

# izberemo vrednost parametra cp, ki ustreza minimalni napaki internega presnega preverjanja
row <- which.min(tab[,"xerror"])
th <- mean(c(tab[row, "CP"], tab[row-1, "CP"]))
#th

# porezemo drevo z izbrano nastavitvijo
rt.model <- prune(rt.model, cp=th)
rpart.plot(rt.model)

predicted <- predict(rt.model, test)
rmse(observed, predicted, mean(train$poraba))  # 0.08810851



###########################
# random forest
###########################
library(randomForest)

rf.model <- randomForest(poraba ~ ., train)
predicted <- predict(rf.model, test)
rmae(observed, predicted, mean(train$poraba))


