corr <- function( directory , threshold = 0){
  
  correlations <- numeric()
  
  for (id in 1:332) {
    nobs <- complete(directory, id)$nobs
    if (nobs > threshold) {
      file <- read.csv(paste0(directory, "/", sprintf("%03d", id), ".csv"))
      corr <- cor(file$sulfate, file$nitrate, use = "complete.obs")
      correlations = c(correlations, corr)
    }
  }
  correlations
}

cr <- corr("specdata", 150)
head(cr)
summary(cr)

cr <- corr("specdata", 400)
head(cr)
summary(cr)

cr <- corr("specdata", 5000)
summary(cr)
length(cr)

cr <- corr("specdata")
summary(cr)
length(cr)
