rankhospital <- function(state, outcome, num = "best") {
  library(dplyr)
  library(data.table)
  ## Read outcome data
  hospital <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  st_unique<-unique(hospital$State)
  test_st<-match(state,st_unique)
  if (is.na(test_st)){
    stop("Invalid state")
  }
  test_oc<-match(outcome,c("heart attack","heart failure","pneumonia"))
  if (is.na(test_oc[1])){
    stop("Invalid outcome")
  }
  ## Return hospital name in that state with the given rank
  df<-data.frame(hospital)
  if (outcome=="heart attack"){
    ha<-df %>% 
      select(State,Hospital.Name,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack) %>%
      filter(State==state,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack!="Not Available") %>%
      arrange(Hospital.Name) %>%
      arrange(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
  }
  if (outcome=="heart failure"){
    ha<-df %>%
      select(State,Hospital.Name,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure) %>%
      filter(State==state,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure!="Not Available") %>%
      arrange(Hospital.Name) %>%
      arrange(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
  }
  if (outcome=="pneumonia"){
    ha<-df %>% 
      select(State,Hospital.Name,Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia) %>%
      filter(State==state,Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia!="Not Available") %>%
      arrange(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
  }
  if (length(ha[,1])<num){
    print(NA)
  }
  if (num=="best"){
    ha[1,2]
  } else if (num=="worst"){
    ha[length(ha[,1]),2]
  } else {
    ha[num,2]
  }
  ## 30-day death rate
}