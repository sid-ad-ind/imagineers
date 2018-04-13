#function to indicate whether file is a true flood or a false flood
labelFlood <- function(filename) {
  data <- read.csv(filename)
  ut <- length(unique(data$Alarm.Tag))
  ar <- length(data$Alarm.Tag)
  p <- (ut/ar)*100
  if (p <= 30){
    return('False flood')
  } else if (p >= 60){
    return('True flood')
  } else {
    randomNumber <- sample(1:2,1,replace=T)
    if (randomNumber == 1) {
      return('True flood')
    } else {
      return('False flood')
    }
  } 
}
#function to create a new output csv file for classification
calculationsInCsv <- function(filename,outputCsvFile) {
  data <- read.csv(filename)
  percentOfUniqueAlarms <- (length(unique(data$Alarm.Tag)) / length(data$Alarm.Tag)) * 100
  natureOfFlood <- labelFlood(filename)
  myData <- cbind(percentOfUniqueAlarms,natureOfFlood)
  write.table( myData,file=outputCsvFile, append = T, sep=',', row.names=F, col.names=F )
}