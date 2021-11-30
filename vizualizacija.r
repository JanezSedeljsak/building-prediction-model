setwd("C:\\Users\\Marko\\Documents\\GitHub\\ai-heating-consumption")
setwd("D:\\ai-heating-consumption")

# pogosto uporabljene funkcije
source("common.r") 

train <- readWithFactorize("ucna.txt")
test <- readWithFactorize("test.txt")

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

plot(train$poraba, train$tedenska_poraba, 
    main="Poraba - 7 dnevna poraba", sub="zelo lepa povezanost med porabo in 7 dnevno porabo",
    xlab="Poraba (kWh)", ylab="7 denvna poraba (kWh)")

plot(train$poraba, train$vcerajsnja_poraba, 
    main="Poraba - Vcerajsnja poraba", sub="pridemo do še lepšega linearnega grafa, z nekaj odstopanji",
    xlab="Poraba (kWh)", ylab="Vcerajsnja poraba (kWh)")

hist(train$poraba, 
    main="Histogram porabe", sub="visok delež na začetku, padec z funkcijo f(x)=1/x. Več meritev ima podpovp. porabo",
    xlab="Poraba", ylab="Število v intervalu")
abline(v=mean(train$poraba), col="red") 
abline(v=median(train$poraba), col="black")

barplot(table(train$namembnost), 
    main="Porazdelitev namembnosti", sub="vidimo, da imamo pri veliki večini podatkov, 'izobrazevalno' namembnost",
    xlab="Namembnost", ylab="Število")

barplot(table(train$vikend), 
    main="Razmerje delovnih dni in vikendov", sub="razmerje med vikendi in denvi v tednu se ujema s pričakovanji (vikendov je 2/7)",
    xlab="Je vikend?", ylab="Število")
abline(h=sum(train$vikend == 1)) # 7036
abline(h=(2/7)*nrow(train), col="red") # 6892.86

pie(table(train$regija), 
    main="Porazdelitev regiji", sub="opazimo, da je porazdelitev dokaj enakomerna")

pie(table(train$sezona),
    main="Porazdelitev sezon", sub="porazdelitev je dokaj enakomerna, imamo manjšo prevlado zime in jeseni")

# Vzeto iz regresija.r
hist(train$temp_zraka, 
    main="Porazdelitev temp. zraka",
    xlab="Temp. zraka", ylab="Število v intervalu")
    
hist(train$temp_rosisca, 
    main="Porazdelitev temp. rosisca",
    xlab="Temp. rosisca", ylab="Število v intervalu")

hist(train$padavine, 
    main="Porazdelitev padavin",
    xlab="Kol. padavin (mm/h)", ylab="Število v intervalu")

hist(train$pritisk, 
    main="Porazdelitev pritiska",
    xlab="Pritisk (mbar)", ylab="Število v intervalu")

hist(log1p(train$hitrost_vetra))
hist(log1p(train$poraba))
hist(log1p(train$povrsina))


#po_letih <- table(train$leto_izgradnje)
#po_letih_iz <- po_letih
#po_letih_iz[names(po_letih_iz)] <- 0
#tmp <- table(train[train$namembnost == "izobrazevalna",]$leto_izgradnje)
#po_letih_iz[names(tmp)] = tmp
#
#barplot(po_letih)
#barplot(po_letih_iz / po_letih)