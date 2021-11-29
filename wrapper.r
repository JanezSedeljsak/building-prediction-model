wrapper <- function(formula, dataset, trainfunc, predictfunc, evalfunc, cvfolds = 10)
{
	df <- model.frame(formula, dataset)
	n <- nrow(df)
	
	cur.formula <- paste(names(df)[1]," ~ ", sep = "")
	candidates <- names(df)[-1]

	global.best <- Inf
	global.formula <- cur.formula

	while(length(candidates))
	{
		selected.att <- 1
		local.best <- Inf

		bucket.id <- rep(1:cvfolds, length.out=n)
		s <- sample(1:n, n, FALSE)
		bucket.id <- bucket.id[s]

		for (i in 1:length(candidates))
		{
			local.formula <- paste(cur.formula, candidates[i], sep = "")
			cat("formula to evaluate:", local.formula, "...\n")
			flush.console()

			cv.results <- vector()
			for (j in 1:cvfolds)
			{	
				sel <- bucket.id == j

				model <- trainfunc(as.formula(local.formula), df[!sel,])
				predicted <- predictfunc(model, df[sel,])
				observed <- df[sel,1]
				trained <- df[!sel,1]

				cv.results[j] <- evalfunc(predicted, observed, trained) 
			}

			local.result <- mean(cv.results)
			
			if (local.result < local.best)
			{
				local.best <- local.result
				selected.att <- i
			}
		}

		cat("selected attribute: ", candidates[selected.att], "\n")

		flush.console()
		
		if (local.best < global.best)
		{
			global.formula <- paste(cur.formula, candidates[selected.att], sep = "")
			global.best <- local.best
		}
		
		cur.formula <- paste(cur.formula, candidates[selected.att], " + ", sep = "")
		candidates <- candidates[-selected.att]
	}

	cat("best model: estimated error = ", global.best,", selected feature subset = ", global.formula, "\n")
}
