complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1
    ## indicating the location of the CSV file
    ##
    ## 'id' is an integer vector indicating the monitor
    ## ID numbers to be used; defaults to 1:332
    ##
    ## Returns a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ......
    ## where 'id' is the monitor ID number, and 'nobs'
    ## is the number of complete cases
    
    if(substr(directory,nchar(directory),nchar(directory)) != "/") {
            directory <- paste(directory,"/",sep="")
    }

    for(i in id) {
        filename <- paste(paste(replicate(3-nchar(i),"0"),collapse=""),i,".csv",sep="")
        tx <- read.csv(paste(directory,filename,sep=""))
        tc <- length(which(complete.cases(tx)==TRUE))
	if(i==id[1]) {
	    nobs <- tc
        } else {
            nobs <- c(nobs,tc)
        }
    }
    data.frame(id,nobs)
}