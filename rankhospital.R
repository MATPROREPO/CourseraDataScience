##Function inputs from the HHS Hospital Compare data set from US DHHS outcome-of-care-measures.csv file
##Inputs a given two character abbreviated state name, outcome name, and the ranking of a hospital in that state
##for that outcome then returns a character vector
##with the lowest 30 day mortality rate for that specified outcome (ties broken by alphabetical order)

rankhospital <- function(state, outcome, num) {
	##Read outcome data
	outcome <- tolower(outcome)
	state <- toupper(state)

	##Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")	
	
	##Check if state and outcome are valid
	if(outcome=="heart attack") {
		colIndex <- 11
	} else if(outcome=="heart failure") {
		colIndex <- 17
	} else if(outcome=="pneumonia") {
		colIndex <- 23
	} else {
		stop("Invalid outcome")
	}

	if(length(data[which(data[,7]==state),])==0) {
		stop("Invalid state")
	}

	##Return hospital name in that state with the lowest 30 day death rate for the specific malady
	data[,colIndex] <- suppressWarnings(as.numeric(data[,colIndex]))
	data <- data[which(data[,7]==state),]
	data <- data[complete.cases(data[,colIndex]),]
	data <- data[order(data[,colIndex],data[,2]),]
	
	n<-length(data[,2])
	
	if(tolower(num)=="best") {
		data[1,2]
	} else if(tolower(num)=="worst") {
		data[n,2]
	} else if(is.numeric(num) & num <= n) {
		data[num,2]
	} else {
		NA
	}

}