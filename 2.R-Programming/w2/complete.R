complete <- function(directory, id = 1:332) {
  library(stringr)
  df <- data.frame()
  for(ids in id) {
    csvfile <- paste(directory, "/", str_pad(ids, 3, pad="0"), ".csv", sep = "")
    dfid <- read.csv(csvfile, skip = 1, header = FALSE)
    dfcsum <- sum(complete.cases(dfid))
    df <- rbind(df, c(ids, dfcsum))
  }
  colnames(df) <- c("id", "nobs")
  df
}
