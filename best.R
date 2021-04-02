best <- function(state, outcome) {
  library(dplyr)
  library(data.table)
  ## Read outcome data
  measures <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  st_unique<-unique(measures$State)
  test_st<-match(state,st_unique)
  if (is.na(test_st)){
    stop("Invalid state")
  }
  test_oc<-match(outcome,c("heart attack","heart failure","pneumonia"))
  if (is.na(test_oc[1])){
    stop("Invalid outcome")
  }
  ## Return hospital name in that state with lowest 30-day death
  df<-data.frame(measures)
  if (outcome=="heart attack"){
    ha<-df %>% 
    select(State,Hospital.Name,Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack) %>%
    filter(State==state,Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack!="Not Available") %>%
    arrange(Hospital.Name) %>%
    arrange(as.numeric(Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
  }
  if (outcome=="heart failure"){
    ha<-df %>%
    select(State,Hospital.Name,Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure) %>%
    filter(State==state,Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure!="Not Available") %>%
    arrange(Hospital.Name) %>%
    arrange(as.numeric(Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
  }
  if (outcome=="pneumonia"){
     ha<-df %>% 
    select(State,Hospital.Name,Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia) %>%
    filter(State==state,Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia!="Not Available") %>%
    arrange(Hospital.Name) %>%
    arrange(as.numeric(Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
  }
  print(ha[1,2])
  ## rate
}