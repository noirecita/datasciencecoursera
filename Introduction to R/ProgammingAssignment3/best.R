best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  validstate <- c(state.abb,"DC", "GU", "PR", "VI")
  if (!is.element(state,validstate)) stop("invalid state")
  validoutcome <- c("heart attack", "heart failure", "pneumonia")
  if (!is.element(outcome,validoutcome)) stop("invalid outcome")
  #heart attack=11 heart failure=17 pneumonia=23
  number = sum(c(11,17,23)*is.element(validoutcome,outcome))
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  data <- subset(data, data$State == state)
  
  data[, number] <- as.numeric(data[, number])
  
  minval <- which(data[, number] == min(data[, number], na.rm=TRUE))
  min(data[minval,2])
}