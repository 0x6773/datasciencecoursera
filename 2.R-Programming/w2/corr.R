corr <- function(directory, threshold = 0) {
  library(stringr)
  ans <- c()
  for(ids in 1:332) {
    csvfile <- paste(directory, "/", str_pad(ids, 3, pad="0"), ".csv", sep = "")
    dfid <- read.csv(csvfile)
    complete_sum <- sum(complete.cases(dfid))
    if(complete_sum > threshold) {
      dfid <- dfid[complete.cases(dfid),]
      correlation <- cor(dfid[c("sulfate", "nitrate")])
      ans <- c(ans, correlation[1,2])
    }
  }
  ans
}
