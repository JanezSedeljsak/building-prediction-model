setwd("C:\\Users\\Marko\\Documents\\GitHub\\ai-heating-consumption")
setwd("D:\\ai-heating-consumption")

# pogosto uporabljene funkcije
source("common.r") 

train <- read.csv("ucna.txt", stringsAsFactors = T)
test <- read.csv("test.txt", stringsAsFactors = T)

##############################
#     Priprava podatkov      #
##############################

# Priprava je sedaj v Parse.java

# install.packages("lubridate")
#library(lubridate)
#
#getSeason <- function(DATES) {
#    WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
#    SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
#    SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
#    FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
#    
#    # Convert dates from any year to 2012 dates
#    d <- as.Date(strftime(DATES, format="2012-%m-%d"))
#    
#    ifelse (d >= WS | d < SE, "Winter",
#            ifelse (d >= SE & d < SS, "Spring",
#                    ifelse (d >= SS & d < FE, "Summer", "Fall")))
#}
#
#ADD_COLUMNS <- function(t) {
#    # dodan atribut ce je vikend
#    t$is_weekend <- wday(t$datum, week_start = 1) > 5
#    
#    # dodan atribut, kateri season je
#    t$season <- getSeason(t$datum)
#    t$season <- factor(t$season, levels=c("Winter", "Spring", "Summer", "Fall"))
#    
#    # sprememba prsenja z -1 na 1mm (privzeta vrednost)
#    t$padavine[t$padavine == -1] <- 1
#    
#    # odstranjen datum (ima index 1)
#    t[-1]
#    
#    # oblacnost je faktor
#    t$oblacnost <- factor(t$oblacnost, levels=seq(0,10,1))
#    
#    # (return)
#    t
#}
#
#train
#
#train <- ADD_COLUMNS(train)
#test <- ADD_COLUMNS(test)

# Racuannje avg iz prejsnjih datumov
#for (date in unique(train$datum)) {
#    dateLast <- as.Date(date)
#    dateFirst <- dateLast - 7
#    avgPoraba <- mean(train[as.Date(train$datum) > dateFirst & as.Date(train$datum) < dateLast, "poraba"])
#    train$prejsnjaPoraba[train$datum == date] <- avgPoraba
#}
#
#for (date in unique(test$datum)) {
#    dateLast <- as.Date(date)
#    dateFirst <- dateLast - 7
#    avgPoraba <- mean(test[as.Date(test$datum) > dateFirst & as.Date(test$datum) < dateLast, "poraba"])
#    test$prejsnjaPoraba[test$datum == date] <- avgPoraba
#}
#
#write.table(train,"C:\\Users\\Marko\\Desktop\\train.txt", append = FALSE, sep = ",", dec = ".", row.names = FALSE, col.names = TRUE)
#write.table(test,"C:\\Users\\Marko\\Desktop\\test.txt", append = FALSE, sep = ",", dec = ".", row.names = FALSE, col.names = TRUE)

factorize <- function (data) {
    data$season <- factor(data$season, levels=c("Winter", "Spring", "Summer", "Autumn"))
    data$oblacnost <- factor(data$oblacnost, levels=seq(0,10,1))
    data$is_weekend <- as.factor(data$is_weekend)
    data
}

train <- factorize(train)
test <- factorize(test)

summary(train)
summary(test)

train$prejsnjaPoraba


##############################
#   Vizualizacija podatkov   #
##############################

plot(train$temp_zraka, train$poraba) # temperature nima bistvenega vpliva na porabo
plot(train$povrsina, train$poraba) # poraba je pricakovano visja glede na povrsino
plot(train$namembnost, train$regija) # pregled stavb po namembnosti glede na regijo
plot(train$namembnost, train$poraba) # kako namembnost vpliva na porabo
plot(train$poraba, train$prejsnjaPoraba)

#library(CORElearn)
#sort(attrEval(namembnost ~ ., train, "InfGain"), decreasing = TRUE)
#
#set.seed(0)
#library(rpart)
#dt <- rpart(regija ~ ., data = train)
#
#library(rpart.plot)
#rpart.plot(dt)
#
#observed <- test$regija
#predicted <- predict(dt, test, type = "class")
#
#tab <- table(observed, predicted)
#
#CA(observed, predicted)


##############################
#  Klasifikacijski problem   #
##############################

set.seed(0)
library(CORElearn)

# Glasovanje
modelDT <- CoreModel(namembnost ~ ., train, model="tree") # 0.45
#modelNB <- CoreModel(namembnost ~ ., train, model="bayes") # 0.35
modelKNN <- CoreModel(namembnost ~ ., train, model="knn", kInNN = 5) # 0.56
modelRT <- CoreModel(namembnost ~ ., train, model="regTree")
modelRF <- CoreModel(namembnost ~ ., train, model="rf")
#modelRFN <- CoreModel(namembnost ~ ., train, model="rfNear")

predDT <- predict(modelDT, test, type = "class")
caDT <- CA(test$namembnost, predDT)
caDT

#predNB <- predict(modelNB, test, type="class")
#caNB <- CA(test$namembnost, predNB)
#caNB

predKNN <- predict(modelKNN, test, type="class")
caKNN <- CA(test$namembnost, predKNN)
caKNN

predRT <- predict(modelRT, test, type="class")
caRT <- CA(test$namembnost, predRT)
caRT

predRF <- predict(modelRF, test, type="class")
caRF <- CA(test$namembnost, predRF)
caRF

#predRFN <- predict(modelRFN, test, type="class")
#caRFN <- CA(test$namembnost, predRFN)
#caRFN

# predikcije modelov
pred <- data.frame(predDT, predKNN, predRF)
head(pred)

predNamembnost <- voting(pred)
predicted <- factor(predNamembnost, levels=levels(train$namembnost))
CA(test$namembnost, predicted)
