corr <- function(directory, threshold = 0){
  
  #initialize a vector to hold resut
  result = vector()
  
  ##read data
  #create vector of all files in directory
  filenames <- list.files(directory)
  
  #loop over filenames
  for (i in filenames) {
    filepath <- file.path(directory, i, fsep = "/")
    
    #read data into data frame
    data <- read.csv(filepath)
    
    #get complete cases logical vector
    data_complete <- complete.cases(data)
    
    #if meet threshold, calculate correlation, else go to next iteration
    if (sum(data_complete) > threshold){
      
      #get vector of sulfate column, and nitrate column
      sulfate <- data["sulfate"]
      nitrate <- data["nitrate"]
      
      #calculate corr
      corr <- round(cor(sulfate, y = nitrate, use = "complete.obs"), digits = 5)
      
      #save correlation value
      result <- c(result, corr)
      
    } 
  }
  
  #return result
  result
}