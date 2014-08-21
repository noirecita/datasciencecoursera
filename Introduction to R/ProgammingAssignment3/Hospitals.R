#Read the data
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

#Plot histogram of the 30-day death rates from heart attack
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])
