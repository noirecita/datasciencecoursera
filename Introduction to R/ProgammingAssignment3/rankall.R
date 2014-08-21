rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that outcome is valid
  validoutcome <- c("heart attack", "heart failure", "pneumonia")
  if (!is.element(outcome,validoutcome)) stop("invalid outcome")
  #heart attack=11 heart failure=17 pneumonia=23
  number = sum(c(11,17,23)*is.element(validoutcome,outcome))
  
  
  ## For each state, find the hospital of the given rank
  #divide data into states
  split = split(data, data$State)
  
  ## Sort hospitals in the given category
  sorted <- lapply(split, sorth2, number=number)
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  create.df(sorted, num)
}

sorth2 <- function(data, number){
  data[, number] <- as.numeric(data[, number])
  #Data Frame Column Slice
  data <- data[c(2,number)]
  #Remove data with NAs
  data = data[complete.cases(data),]
  #sort
  data[order(data[,2], data[,1]), ]
}

create.df <- function(sorted, num){
    x <- character(0)
    y <- character(0)
    for (i in 1:54) {
      x[i] <- labels(sorted)[i]
      ## Return hospital name in that state with the given rank
      if (num =="best") {
        y[i] <-sorted[[i]][1,1]
      } else if (num =="worst") {
        y[i] <-sorted[[i]][nrow(sorted[[i]]),1]
      #If num > number of hospitals in state, then return NA
      }else if (num > nrow(sorted[[i]])) {
        y[i] <- NA
      }else y[i] <- sorted[[i]][num,1]
    }
    data.frame(hospital=y, state=x, stringsAsFactors=FALSE)
  }