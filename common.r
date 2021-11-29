CA <- function(obs, pred) {
	tab <- table(obs, pred)
	sum(diag(tab)) / sum(tab)
}

voting <- function(predictions) {
	res <- vector()

  	for (i in 1 : nrow(predictions))  	
	{
		vec <- unlist(predictions[i,])
    	res[i] <- names(which.max(table(vec)))
  	}

  	res
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