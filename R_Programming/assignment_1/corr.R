## source("complete.R")

corr <- function(directory, threshold=0) {
	## get all complete data values for the directory (uses fcn from complete.R)
	completeObsPerMonitor <- complete(directory)
	## subset to those with # of obs above threshold
	nobsOverThreshold <- completeObsPerMonitor$nobs > threshold
	monitorsAboveThreshold <- completeObsPerMonitor[nobsOverThreshold, ]
	
	files <- list.files(directory, full.names=TRUE)	
	correlations <- numeric(0)

	for(monitorId in monitorsAboveThreshold$id) {		
		data <- read.csv(files[monitorId])
		
		## get correlation for sulfate/nitrate for complete pairs ONLY
		correlation <- cor(data$sulfate, data$nitrate, use="pairwise.complete.obs")
		correlations <- c(correlations, correlation)
	}
	correlations
 }