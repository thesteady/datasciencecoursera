rankall <- function(outcome, num = "best") {
	## check input validity
	if( !outcomeValid(outcome) ) {
		stop('invalid outcome')
	}
	data <- read.csv('outcome-of-care-measures.csv', colClasses='character')

	##Figure out what the column name should be
	column <- generateColName(outcome)

	## strip data down to just what I need and split by state
	data <- data[c("Hospital.Name", "State", column)]
	dataByState <- split(data, data$State)
	
	rankHospitals <- function(stateData, num) {
		## coerce the "Not Available" vals to NA
		stateData[,3] <- as.numeric(stateData[,3])
		
		decreasingVal <- FALSE
		if(num == "worst") {
			num <- 1
			decreasingVal <- TRUE
		}
		if(num == "best") {
			num <- 1
		}
		
		##order data by the outcome column
		sortedData <- stateData[ order(
									stateData[column],
									stateData$Hospital.Name,
									decreasing=decreasingVal,
									na.last=TRUE
								), 1:2]
								
		## return the hospital at that value
		sortedData$Hospital.Name[num]
	}

	results <- lapply(dataByState, rankHospitals, num)	
	data.frame(hospital=unlist(results), state=names(results), row.names = names(results))
}


generateColName <- function(outcome) {
	## generate col suffix
	splitStr <- strsplit(outcome, " ")[[1]]
	colSuffix <- paste(
					toupper(substring(splitStr, 1,1)),
					substring(splitStr, 2),
					sep="",
					collapse="."
				)
	## join the suffix with the prefix together
	paste("Hospital.30.Day.Death..Mortality..Rates.from", colSuffix, sep=".")
}


outcomeValid <- function(outcome) {
	## Returns true if outcome provided matches the valid outcomes we can use
	## otherwise returns false

	validOutcomes <- c('heart attack', 'heart failure', 'pneumonia')
	outcome %in% validOutcomes 
}