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

summary(train)
summary(test)

train$prejsnjaPoraba


##############################
#   Vizualizacija podatkov   #
##############################

library(rpart.plot)

plot(train$temp_zraka, train$poraba, 
    main="Temp - poraba", sub="temperature nima bistvenega vpliva na porabo",
    xlab="Temp. zraka (°C)", ylab="Poraba (kWh)")

plot(train$povrsina, train$poraba, 
    main="Povrsina - Poraba", sub="poraba je pricakovano visja glede na povrsino",
    xlab="Povrsina (m^2)", ylab="Poraba (kWh)") 

plot(train$namembnost, train$regija, 
    main="Namembnost - Regija", sub="pregled stavb po namembnosti glede na regijo",
    xlab="Namembnost", ylab="Regija") 

plot(train$namembnost, train$poraba, 
    main="Namembnost - Poraba", sub="kako namembnost vpliva na porabo",
    xlab="Povrsina (m^2)", ylab="Poraba (kWh)") 

plot(train$poraba, train$prejsnjaPoraba, 
    main="Poraba - 7 dnevna poraba", sub="zelo lepa povezanost med porabo in 7 dnevno porabo",
    xlab="Poraba (kWh)", ylab="7 denvna poraba (kWh)")

hist(train$poraba) # visok delež na začetku, padec z funkcijo f(x)=1/x
abline(v=mean(train$poraba), col="red") 
abline(v=median(train$poraba), col="black") # več meritev imamo z podpovp. porabo

barplot(table(train$namembnost)) # vidimo, da imamo pri veliki večini podatkov, "izobrazevalno" namembnost
barplot(table(train$is_weekend)) # razmerje med vikendi in denvi v tednu se ujema s pričakovanji (vikendov je 2/7)
abline(h=sum(train$is_weekend == 1)) # 7036
abline(h=(2/7)*nrow(train), col="red") # 6892.86

pie(table(train$regija)) # porazdelitev regije vzhodna, zahodna je dokaj enakomerna
pie(table(train$season)) # ponovno dokaj lepa porazdelitev, imamo manjšo prevlado podatkov iz zime

by_year <- table(train$leto_izgradnje)
by_year_education <- table(train[train$namembnost == "izobrazevalna",]$leto_izgradnje)
ratio <- cbind(by_year[1], by_year[-1]/by_year_education[-1])
barplot(by_year_education) # število izgradenj glede na leto

##############################
#  Klasifikacijski problem   #
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
#modelNB <- CoreModel(namembnost ~ ., train, model="bayes") # 0.35
modelKNN <- CoreModel(namembnost ~ ., train, model="knn", kInNN = 5) # 0.56
#modelRT <- CoreModel(namembnost ~ ., train, model="regTree")
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

#predRT <- predict(modelRT, test, type="class")
#caRT <- CA(test$namembnost, predRT)
#caRT

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
