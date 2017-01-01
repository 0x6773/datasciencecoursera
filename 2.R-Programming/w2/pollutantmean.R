pollutantmean <- function(directory, pollutant, id = 1:332) {
  library(stringr)
  df <- data.frame()
  for(ids in id) {
    csvfile <- paste(directory, "/", str_pad(ids, 3, pad="0"), ".csv", sep = "")
    dfid <- read.csv(csvfile, skip = 1, header = FALSE)
    df <- rbind(df, dfid)
  }
  colnames(df) <- c("Date","sulfate","nitrate","ID")
  mean(df[[pollutant]], na.rm = TRUE)
}
