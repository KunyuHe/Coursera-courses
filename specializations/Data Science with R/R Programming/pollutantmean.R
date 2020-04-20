setwd("C:/Users/Administrator/Desktop/Coursera/Data Science/R/specdata/")
pollutantmean <- function(pollutant, id = 1:332)
{
  means <- vector("list", length(id))
  for(i in id){
    if(i < 10){
      a <- read.csv(paste("00", as.character(i), ".csv", sep = ""))
      }
    else if(i>=10 & i<100){
      a <- read.csv(paste("00", as.character(i), ".csv", sep = ""))
      }
    else{
      a <- read.csv(paste("00", as.character(i), ".csv", sep = ""))
      }
    means[[i]] <- mean(a$pollutant, na.rm = TRUE)
  }
  mean(means)
}
pollutantmean("sulfate", 1:10)