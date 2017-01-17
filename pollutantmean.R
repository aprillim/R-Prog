pollutantmean <- function(directory, pollutant, id = 1:332) {

  ##get a list of filenames
  ##filenames <- list.files(path = directory)
  
  ##initialize a vector to hold values
  vals <- vector()
  
  ##read the data into data frame
  
  ##loop over all ids
  for (i in id){
    
    ##convert id into 3 digits
    id_num <- sprintf("%03d.csv", i)
    
    filepath <- file.path(directory, id_num, fsep ="/")
    
    ##read data. This will give a data frame. 
    data <- read.csv(filepath)
    
    ##select our column. This will give a vector.
    col <- data[,pollutant]
    
    ##ignore NA. This will give a vector of only non NA values.
    col <- col[!is.na(col)]
    
    ##append desired values to values vector
    
    vals <- c(vals, col)
    }
  ##return the mean of vals
  
  round(mean(vals), 3)
}