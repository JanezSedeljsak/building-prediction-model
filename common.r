NAMEMBNOST_LEVELS <- c("izobrazevalna", "javno_storitvena", "kulturno_razvedrilna", "poslovna", "stanovanjska")

CA <- function(obs, pred) {
	tab <- table(obs, pred)
	sum(diag(tab)) / sum(tab)
}

voting <- function(predictions) {
	res <- vector()
  for (i in 1 : nrow(predictions)) {
		vec <- unlist(predictions[i,])
    res[i] <- names(which.max(table(vec)))
  }

  res
}

brier.score <- function(obsMat, predMat)
{
	sum((obsMat - predMat) ^ 2) / nrow(predMat)
}

namembnostModelStats <- function(actual, predicted, ignoreMatrix=F) {
  print(paste("CA:    ", paste(" ", CA(actual, predicted))))
  if (ignoreMatrix != T) {
    confusionMatrix(factor(predicted, levels=NAMEMBNOST_LEVELS), actual)
  }
}

factorize <- function (data) {
    data$namembnost <- factor(data$namembnost, levels=NAMEMBNOST_LEVELS)
    data$sezona <- factor(data$sezona, levels=c("zima", "spomlad", "poletje", "jesen"))
    data$regija <- as.factor(data$regija)
    data$oblacnost <- factor(data$oblacnost, levels=seq(0,10,1))
    data$vikend <- as.factor(data$vikend)
    data$mesec <- factor(data$mesec, levels=seq(1,12,1))
    data$tedenska_poraba[data$tedenska_poraba == -1] <- NA
    data$vcerajsnja_poraba[data$vcerajsnja_poraba == -1] <- NA
    data
}

readWithFactorize <- function (filename) {
	factorize(read.csv(filename, stringsAsFactors = T))
}

# srednja absolutna napaka
mae <- function(obs, pred)
{
  mean(abs(obs - pred))
}

# srednja kvadratna napaka
mse <- function(obs, pred)
{
  mean((obs - pred)^2)
}

# relativna srednja absolutna napaka
rmae <- function(obs, pred, mean.val) 
{  
  sum(abs(obs - pred)) / sum(abs(obs - mean.val))
}

# relativna srednja kvadratna napaka
rmse <- function(obs, pred, mean.val) 
{  
  sum((obs - pred)^2)/sum((obs - mean.val)^2)
}

myTrainFunc <- function(formula, traindata)
{
	rpart(formula, traindata)	
}

# Funkcija za pridobivanje napovedi modela (razredi)
myPredictFunc <- function(model, testdata)
{
	predict(model, testdata, type="class")
}



# Atribute lahko izberemo glede na klasifikacijsko tocnost modela
myEvalFunc <- function(predicted, observed, trained)
{
	1.0 - mean(observed == predicted)	
}