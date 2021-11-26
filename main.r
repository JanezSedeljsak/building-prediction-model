setwd("D:\\ai-heating-consumption")

train <- read.table("ucnaSem1.txt", header = T, sep = ",", stringsAsFactors = T)
test <- read.table("testnaSem1.txt", header = T, sep = ",", stringsAsFactors = T)
summary(train)

set.seed(0)

library(rpart)
dt <- rpart(regija ~ ., data = train)

library(rpart.plot)
rpart.plot(dt)

observed <- test$regija
predicted <- predict(dt, test, type = "class")

tab <- table(observed, predicted)

CA <- function(obs, pred) {
	tab <- table(obs, pred)
	sum(diag(tab)) / sum(tab)
}

CA(observed, predicted)
