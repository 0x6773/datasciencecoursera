rankall <- function(outcome, num = "best") {
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  hpotl <- list(
    "heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
    "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
    "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  
  if(!outcome %in% names(hpotl)) {
    stop("invalid outcome")
  }
  
  hpot <- hpotl[[outcome]]
  df <- df[c("Hospital.Name", "State", hpot)]
  
  useme <- function(dt) {
    dt[hpot] <- as.numeric(dt[[hpot]])
    nux <- 0
    ret <- NULL
    
    if(num == "best") {
      dt <- dt[order(dt[,3], dt[,1]),]
      ret <- dt[1,]$Hospital.Name
    } else if(num == "worst") {
      dt <- dt[order(-dt[,3], dt[,1]),]
      ret <- dt[1,]$Hospital.Name
    } else {
      dt <- dt[order(dt[,3], dt[,1]),]
      ret <- dt[num,]$Hospital.Name
    }
    ret
  }
  
  sp <- split(df, df$State)
  size <- 54
  ans <- data.frame(hospital = character(size), state = character(size), stringsAsFactors = FALSE)
  for(i in 1:size) {
    ans$hospital[i] <- toString(i)
    ans$state[i] <- toString(i)
  }
  states <- names(sp)
  for(i in 1:size) {
    state <- states[i]
    hosp <- useme(sp[[state]])
    ans$hospital[i] <- hosp
    ans$state[i] <- state
  }
  ans
}
