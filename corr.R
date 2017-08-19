corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1
    ## indicating the location of the CSV file
    ##
    ## 'threshold' is a numeric vector of length 1
    ## indicating the number of completely observed
    ## observations (on all variables) required to 
    ## compute the correlation between
    ## nitrate and sulfate; the default is 0
    ##
    ## Returns a numeric vector of correlations
    ## Results are unrounded

    if(substr(directory,nchar(directory),nchar(directory)) != "/") {
            directory <- paste(directory,"/",sep="")
    }

    cmp <- complete(directory)
    id <- cmp[which(cmp$nobs>=threshold),"id"]
    
    corrVals <- numeric(0)

    for(i in id) {
        filename <- paste(paste(replicate(3-nchar(i),"0"),collapse=""),i,".csv",sep="")
        tx <- read.csv(paste(directory,filename,sep=""))
	tc <- cor(tx$nitrate,tx$sulfate,use="na.or.complete")
	if(i==id[1]) {
	    corrVals <- tc
        } else {
            corrVals <- c(corrVals,tc)
        }
    }
    corrVals
}