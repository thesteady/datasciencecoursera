pollutantmean <- function(directory, pollutant, id=1:332) {
    allValues <- numeric(0)
		
	for(idNum in id) {
		filepath <- generateFilePath(directory, idNum)
		csv <- read.csv(filepath)
		values <- csv[pollutant][!is.na(csv[pollutant])]
        allValues <- c(allValues, values)
	}
	mean(allValues)
}

generateFilePath <- function(directory, id) {
	paste(directory, '/', formatId(id), '.csv', sep="")
}

formatId <- function(id) {
	## formats id number to match how the files are named.
	if(id < 10) {
		paste("00", id, sep="")
	} else if(id < 100) {
		paste("0", id, sep="")
	} else {
		as.character(id)
	}
}