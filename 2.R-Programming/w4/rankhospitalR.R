rankhospital <- function(state, outcome, num = "best") {
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  df <- df[df$State == state,]
  if(nrow(df) == 0) {
    stop("invalid state")
  }
  hpotl <- list(
    "heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
    "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
    "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  
  if(!outcome %in% names(hpotl)) {
    stop("invalid outcome")
  }

  hpot <- hpotl[[outcome]]
  dt <- df[c("Hospital.Name", hpot)]
  dt[hpot] <- as.numeric(dt[[hpot]])
  dt <- dt[complete.cases(dt),]
  
  dt <- dt[order(dt[,2], dt[,1]),]
  
  if(!is.numeric(num)) {
    if(num == "best") {
      num <- 1
    } else {
      num <- nrow(dt)
    }
  }
  
  dt[num,]$Hospital.Name
}
