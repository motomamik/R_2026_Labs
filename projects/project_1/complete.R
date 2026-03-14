complete <- function( directory , id = 1:332 ){
  
  data <- data.frame(id = numeric(), nobs = numeric())
  
  for (i in id) {
    file <- read.csv(paste0(directory, "/", sprintf("%03d", i), ".csv"))
    nobs <- sum(!is.na(file$sulfate) & !is.na(file$nitrate))
    #nobs <- sum(complete.cases(file))
    row <- data.frame(id = i, nobs = nobs)
    data <- rbind(data, row)
  }
  data
}

complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)

