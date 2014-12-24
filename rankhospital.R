rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        valid.outcome <- FALSE
        valid.state   <- FALSE
        allowed.outcomes <- c("heart attack", "pneumonia", "heart failure")
        for(o in allowed.outcomes) {
                if(outcome == o){
                        valid.outcome <- TRUE  
                }
        }
        if(!valid.outcome) {
                stop("invalid outcome")
        }
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        allowed.states <- unique(data$State)
        for(s in allowed.states) {
                if(s == state){
                        valid.state <- TRUE
                }
        }
        if(!valid.state){
                stop("invalid state")
        }
        #look at lines 11 = heart attack, 17 = heart failure, 23 = pneumonia
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
        
        #get all the hostpitals for the wanted state
        data <- subset(data, data$State == state)
        #simplify the data frame
        data <- cbind(data[2], data[outcome.value])
        data[,2] <- as.numeric(data[,2])
        data <- data[complete.cases(data), ]
        #order the data by the now numeric outcome column and return the top name only
        data <- data[order(data[2], data[1]), ]
        
        if(num == "best"){
                return(data[1,1])
        }
        if(num == "worst"){
                return(data[nrow(data),1])  
        }
        data[num,1]
}
