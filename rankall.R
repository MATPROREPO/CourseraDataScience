##Function inputs from the HHS Hospital Compare data set from US DHHS outcome-of-care-measures.csv file
##Inputs a given two character abbreviated state name, outcome name, and the ranking of a hospital in that state
##for that outcome then returns a character vector
##with the lowest 30 day mortality rate for that specified outcome (ties broken by alphabetical order)

rankall <- function(outcome, num) {
	##Read outcome data
	outcome <- tolower(outcome)

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


	data[,colIndex] <- suppressWarnings(as.numeric(data[,colIndex]))
	data <- data[complete.cases(data[,colIndex]),]
	state <- sort(unique(data[,7]))

	for(i in state) {
	
		##Return hospital name in that state with the lowest 30 day death rate for the specific malady
		temp <- data[which(data[,7]==i),]
		temp <- temp[order(temp[,colIndex],temp[,2]),]
		
		n<-length(temp[,2])
		
		if(tolower(num)=="best") {
			if(i==state[1]) {
				hospital <- temp[1,2]
			} else {
				hospital <- c(hospital,temp[1,2])
			}
		} else if(tolower(num)=="worst") {
			if(i==state[1]) {
				hospital <- temp[n,2]
			} else {
				hospital <- c(hospital,temp[n,2])
			}
		} else if(is.numeric(num) & num <= n) {
			if(i==state[1]) {
				hospital <- temp[num,2]
			} else {
				hospital <- c(hospital,temp[num,2])
			}
		} else {
			if(i==state[1]) { 
				hospital <- NA
			} else {
				hospital <- c(hospital,NA)
			}
		}

	}
	data.frame(state,hospital)	
}