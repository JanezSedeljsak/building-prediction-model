setwd("D:\\ai-heating-consumption")

train <- read.csv("ucnaSem1.txt", stringsAsFactors = T)
test <- read.csv("testnaSem1.txt", stringsAsFactors = T)

# install.packages("lubridate")
library(lubridate)

IS_WEEKEND <- function(d) {
    week_day <- wday(d)
    ifelse(week_day == 1 | week_day == 7, 1, 0)
}

GET_SEASON <- function(d) {
    numeric_date <- 100 * month(d) + day(d)
    cuts <- base::cut(numeric_date, breaks = c(0,319,0620,0921,1220,1231))
    levels(cuts) <- c("Winter","Spring","Summer","Fall","Winter")
    cuts
}

ADD_COLUMNS <- function(t) {
    dates <- t$datum
    weeked_rows <- c()
    season_rows <- c()
    i <- 1
    for (d in dates) {
        weeked_rows[i] <- IS_WEEKEND(d)
        season_rows[i] <- GET_SEASON(d)
        i <- i + 1
    }

    t$is_weekend <- weeked_rows
    t$season <- season_rows
    t$stavba <- NULL
    t$padavine[t$padavine == -1] <- 1
    t
}

train <- ADD_COLUMNS(train)
test <- ADD_COLUMNS(test)
#summary(train)

library(CORElearn)
sort(attrEval(namembnost ~ ., train, "InfGain"), decreasing = TRUE)

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
