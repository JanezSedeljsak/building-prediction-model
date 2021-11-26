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