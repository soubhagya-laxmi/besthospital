
best <- function(state, outcome)
{
  
  dat<- read.csv("outcome-of-care-measures.csv", colClasses = "character",na.strings="Not Available")
  
  validOutcome <- c("heart attack","heart failure","pneumonia")
  if(!outcome %in% validOutcome)
  { stop("Invalid Outcome") }
  
  validState <- unique(dat[,7])
  if(!state %in% validState)
  {stop("Invalid state")}
  
  fullColName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  colName <- fullColName[match(outcome,validOutcome)]
  
  
  k <- dat[dat$State==state,]
  idx <- which.min(as.double(k[,colName]))
  
  k[idx,"Hospital.Name"]
}

