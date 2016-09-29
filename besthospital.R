## best function take two arguments: name of a state and an outcome name
## The function reads the outcome-of-care-measures.csv
## returns a character vector with the name of the hospital 
## The hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome in that state.

## The outcomes can be one of "heart attack", "heart failure", or "pneumonia".

best <- function(state, outcome)
{
  ## Read outcome data
  dat<- read.csv("outcome-of-care-measures.csv", colClasses = "character",na.strings="Not Available")
  
  ## Check that state and outcome are valid
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
  
  ## Return hospital name in that state with lowest 30-day death rate
  k[idx,"Hospital.Name"]
}

