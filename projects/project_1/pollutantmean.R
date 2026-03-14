pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  data <- data.frame()
  
  if (!pollutant %in% c("sulfate", "nitrate")) {
    print("Not a pollutant")
    return(NULL)
  }
  
  for (i in id) {
    file <- read.csv(paste0(directory, "/", sprintf("%03d", i), ".csv"))
    data <- rbind(data, file)
  }
  
  mean <- mean(data[[pollutant]], na.rm = TRUE)
  mean
}

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)