best <- function(state, outcome) {
    
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
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    if (outcome==outcome_condition[1]){
        #Heart Attack - Column 11
        outcome_state<-outcome_data[,11][outcome_data$State==state]
        min_mort<-min(outcome_state)
        hos_list<-outcome_data[,2][outcome_data$State==state][min_mort==outcome_state]
    }
    else if (outcome==outcome_condition[2]){
        #Heart Failure - Column 17
        outcome_state<-outcome_data[,17][outcome_data$State==state]
        min_mort<-min(outcome_state)
        hos_list<-outcome_data[,2][outcome_data$State==state][min_mort==outcome_state]
    }
    else{
        #Pneumonia - Column 23
        outcome_state<-outcome_data[,23][outcome_data$State==state]
        min_mort<-min(outcome_state,na.rm = TRUE)
        hos_list<-outcome_data[,2][outcome_data$State==state][min_mort==outcome_state]
    }
    
    sort(hos_list)[1]
}