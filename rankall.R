rankall <- function(outcome, num = "best") {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        valid.outcome <- FALSE
        allowed.outcomes <- c("heart attack", "pneumonia", "heart failure")
        for(o in allowed.outcomes) {
                if(outcome == o){
                        valid.outcome <- TRUE  
                }
        }
        if(!valid.outcome) {
                stop("invalid outcome")
        }
        outcome.value <- 0
        if(outcome == "heart attack") {
                outcome.value <- 11
        }
        else if (outcome == "heart failure") {
                outcome.value <- 17
        }
        else { #pneumonia
                outcome.value <- 23
        }
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        data <- cbind(data[2], data[7], data[outcome.value])
        data[,3] <- as.numeric(data[,3])
        data <- data[complete.cases(data), ]
        colnames(data)[1] <- "Name"
        colnames(data)[2] <- "State"
        colnames(data)[3] <- "Outcome"
        splits <- split(data, data$State)
        
        rank <- data.frame()
        for(s in splits) {
                s <- s[order(s$Outcome, s$Name), ]
                #message(s)
                if(num == "best"){
                        rank <- rbind(rank, data.frame(s[1,1], s[1,2]))
                }
                else if(num == "worst"){
                        rank <- rbind(rank, data.frame(s[nrow(s),1], s[1,2]))  
                }
                else {
                        rank <- rbind(rank, data.frame(s[num, 1], s[1,2]))
                }
        }
        colnames(rank)[1] <- "hospital"
        colnames(rank)[2] <- "state"
        rank
}