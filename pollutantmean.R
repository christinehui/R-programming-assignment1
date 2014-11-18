#read all data in csv into all_data then flatten the list
readAllcsv <- function (directory, file_type="*.csv") {
        files <- Sys.glob(file.path(getwd(),directory,file_type))
        
        listOfFiles <- lapply(files, function(x) read.csv(x, header=TRUE))
        
        #listOfFiles returns a list of list: list of data in each csv file
        #Flatten the list of list into one single list and stores into merged_list 
        merged_list <- list()
        len <- length(listOfFiles)
        for (i in seq_len(len)) {
                merged_list <- rbind(listOfFiles[[i]], merged_list, deparse.level=0)
        }
        merged_list
}

#get the rows with ID equals to the id argument range and stores into range_data
getDataOfID <- function (all_data, id) {
        range_data <- list()
        for (i in id) {
                spec_id_data <- all_data[all_data$ID==i,]
                range_data <- rbind(range_data, spec_id_data, deparse.level=0)
        }
        range_data
}

pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
    
        all_data <- readAllcsv(directory)
        
        range_data <- getDataOfID(all_data,id)

        
        #get the mean of the specific pollutant and round to 3 decimal places
        round(sapply(range_data[pollutant],function (x) mean(x,na.rm=TRUE)),3)
        
}