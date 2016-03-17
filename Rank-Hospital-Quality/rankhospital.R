rankhospital <- function(state, outcome, num = "best") {
    
    ## Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    outcome_data[,11]<-suppressWarnings(as.numeric(outcome_data[,11]))
    outcome_data[,17]<-suppressWarnings(as.numeric(outcome_data[,17]))
    outcome_data[,23]<-suppressWarnings(as.numeric(outcome_data[,23]))
    good <- complete.cases(outcome_data)
    outcome_data <- outcome_data[good,]
    
    ## Check that state and outcome are valid
    outcome_condition<-c("heart attack","heart failure","pneumonia")
    if (sum(outcome_data[,7]==state)==0){
        stop("invalid state")
    }
    if (sum(outcome_condition==outcome)==0){
        stop("invalid outcome")
    }
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    if (outcome==outcome_condition[1]){
        #Heart Attack - Column 11
        outcome_state<-outcome_data[,11][outcome_data$State==state]
        hos_list<-outcome_data[,2][outcome_data$State==state]
        ord<-order(outcome_state,hos_list)
        outcome_state<-outcome_state[ord]
        hos_list<-hos_list[ord]
    }
    else if (outcome==outcome_condition[2]){
        #Heart Failure - Column 17
        outcome_state<-outcome_data[,17][outcome_data$State==state]
        hos_list<-outcome_data[,2][outcome_data$State==state]
        ord<-order(outcome_state,hos_list)
        outcome_state<-outcome_state[ord]
        hos_list<-hos_list[ord]
    }
    else{
        #Pneumonia - Column 23
        outcome_state<-outcome_data[,23][outcome_data$State==state]
        hos_list<-outcome_data[,2][outcome_data$State==state]
        ord<-order(outcome_state,hos_list)
        outcome_state<-outcome_state[ord]
        hos_list<-hos_list[ord]
    }
    
    if(num=="best"){
        num <- 1
    }
    else if (num=="worst"){
        num<-length(hos_list)
    }
    
    rank_hos<-hos_list[num]
    rank_hos
}