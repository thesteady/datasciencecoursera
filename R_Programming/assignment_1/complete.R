complete <- function(directory, id = 1:332) {
	data <- data.frame()
	for(idNum in id) {
		filePath <- generateFilePath(directory, idNum)
		csv <- read.csv(filePath)
		numNobs <- sum(complete.cases(csv[,2:3]))
		data <- rbind(data, c(idNum, numNobs))
	}
	colnames(data)<- c('id', 'nobs')
	data
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