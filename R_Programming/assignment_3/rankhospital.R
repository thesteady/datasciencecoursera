rankhospital <- function(state, outcome, num="best") {
	## Read outcome data
	data <- read.csv('outcome-of-care-measures.csv', colClasses='character')
	
	## Check state and outomce are valid
	if( !stateValid(state, data) ) {
		stop('invalid state')
	}
	if( !outcomeValid(outcome) ) {
		stop('invalid outcome')
	}
	outcomeColName <- generateColName(outcome)
	
	##create ranked data for the outcome
	data <- data[data$State == state,]
	data <- data[c("Hospital.Name", outcomeColName)]
	data[,2] <- as.numeric(data[,2])
	data <- data[complete.cases(data),]
	data <- data[order(data[outcomeColName], data$Hospital.Name), 1:2]
	
	## transform num from best/worst (worst being last in rankings)
	if(num == "best") {
		num <- 1
	}else if(num == "worst") {
		num <- nrow(data)
	}
	## Return hospital name in the state with given rank or NA
	data$Hospital.Name[num]
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


stateValid <- function(state, data) {
	## Returns true if state is represented in the dataset
	## otherwise returns false
	validStates <- unique(data$State)
	state %in% validStates
}


outcomeValid <- function(outcome) {
	## Returns true if outcome provided matches the valid outcomes we can use
	## otherwise returns false

	validOutcomes <- c('heart attack', 'heart failure', 'pneumonia')
	outcome %in% validOutcomes 
}