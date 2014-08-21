rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  validstate <- c(state.abb,"DC", "GU", "PR", "VI")
  if (!is.element(state,validstate)) stop("invalid state")
  validoutcome <- c("heart attack", "heart failure", "pneumonia")
  if (!is.element(outcome,validoutcome)) stop("invalid outcome")
  #heart attack=11 heart failure=17 pneumonia=23
  number = sum(c(11,17,23)*is.element(validoutcome,outcome))
  
  ## Sort hospitals in the given category
  sorted <- sorth(state,data,number)
  
  ## Return hospital name in that state with the given rank
  if (num =="best") return(sorted[[1,1]])
  if (num =="worst") return(sorted[[nrow(sorted),1]])
  #If num > number of hospitals in state, then return NA
  if (num > nrow(sorted)) return(NA)
  sorted[[num,1]]
}

sorth <- function(state,data,number){
  data <- subset(data, data$State == state)
  data[, number] <- as.numeric(data[, number])
  #Data Frame Column Slice
  data <- data[c(2,number)]
  #Remove data with NAs
  data = data[complete.cases(data),]
  #sort
  data[order(data[,2], data[,1]), ]
}
