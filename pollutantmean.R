pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1
    ## indicating the location of the CSV file
    ##
    ## 'pollutant' is a character vector of length 1 
    ## indicating the name of the pollutant for which 
    ## we will calculate the mean; either "sulfate" or 
    ## "nitrate"
    ##
    ## 'id' is an integer vector indicating the monitor
    ## ID numbers to be used; defaults to 1:332
    ##
    ## Return the mean of the pollutant across all monitors 
    ## listed within the 'id' vector (ignoring NA values)
    ## and returns an unrounded result
    
    if(substr(directory,nchar(directory),nchar(directory)) != "/") {
            directory <- paste(directory,"/",sep="")
    }
    
    totalSum <- 0.0
    totalCount <- 0
	
    for(i in id) {
        filename <- paste(paste(replicate(3-nchar(i),"0"),collapse=""),i,".csv",sep="")
        tx <- read.csv(paste(directory,filename,sep=""))
	totalSum <- totalSum + sum(tx[[pollutant]],na.rm=TRUE)
	totalCount <- totalCount + length(which(!is.na(tx[[pollutant]])))
    }
    totalMean <- totalSum / totalCount
    totalMean
}