rankall <- function(outcome, num = "best") {
    
    ## Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    outcome_data[,11]<-suppressWarnings(as.numeric(outcome_data[,11]))
    outcome_data[,17]<-suppressWarnings(as.numeric(outcome_data[,17]))
    outcome_data[,23]<-suppressWarnings(as.numeric(outcome_data[,23]))
    good <- complete.cases(outcome_data)
    outcome_data <- outcome_data[good,]
    
    #Identify the states from the data
    state_list<-factor(outcome_data[,7])
    states<-levels(state_list)
    
    ## Check that state and outcome are valid
    outcome_condition<-c("heart attack","heart failure","pneumonia")
    if (sum(outcome_condition==outcome)==0){
        stop("invalid outcome")
    }
    
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    #initialize rank_hos
    rank_hos<-c()
    
    if (outcome==outcome_condition[1]){
        for(i in 1:length(states)){
            #Heart Attack - Column 11
            outcome_state<-outcome_data[,11][outcome_data$State==states[i]]
            hos_list<-outcome_data[,2][outcome_data$State==states[i]]
            ord<-order(outcome_state,hos_list)
            outcome_state<-outcome_state[ord]
            hos_list<-hos_list[ord]
            if(num=="best"){
                num <- 1
            }
            else if (num=="worst"){
                num<-length(hos_list)
            }
            rank_hos[i]<-hos_list[num]
        }
    }
    else if (outcome==outcome_condition[2]){
        for(i in 1:length(states)){
            #Heart Failure - Column 17
            outcome_state<-outcome_data[,17][outcome_data$State==states[i]]
            hos_list<-outcome_data[,2][outcome_data$State==states[i]]
            ord<-order(outcome_state,hos_list)
            outcome_state<-outcome_state[ord]
            hos_list<-hos_list[ord]
            if(num=="best"){
                num <- 1
            }
            else if (num=="worst"){
                num<-length(hos_list)
            }
            rank_hos[i]<-hos_list[num]
        }
    }
    else{
        for(i in 1:length(states)){
            #Pneumonia - Column 23
            outcome_state<-outcome_data[,23][outcome_data$State==states[i]]
            hos_list<-outcome_data[,2][outcome_data$State==states[i]]
            ord<-order(outcome_state,hos_list)
            outcome_state<-outcome_state[ord]
            hos_list<-hos_list[ord]
            if(num=="best"){
                num <- 1
            }
            else if (num=="worst"){
                num<-length(hos_list)
            }
            rank_hos[i]<-hos_list[num]
        }
    }
    rank_all<-data.frame(hospital=rank_hos,state=states)
}