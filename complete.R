complete <- function(directory, id = 1:332){

  ##initialize a data frame to store result
  result <- data.frame()
  
  
  ##read data
  ##loop over ids
  for(i in id){
    
    #create 3digit filename 
    filename <- sprintf("%03d.csv", i)
    filepath <- file.path(directory, filename, fsep = "/")
    
    #save data into data frame
    data = read.csv(filepath)
    
    #num of complete cases
    num <- sum(complete.cases(data))
    
    #create vector
    new_row <- c(i, num)
    
    #append vector to data frame
    result <- rbind(result, new_row)
  }
   colnames(result) <- c("id", "nobs")
   result
}