rankall <- function(outcome, num = "best") {
  library(dplyr)
  library(data.table)
  ## Read outcome data
  rank_hosp <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  #Get total states in US and sort by order
  st_unique<-unique(rank_hosp$State)
  st_unique<-sort(st_unique)
  
  #create a new data frame
  hospital<-c()
  state_dt<-c()
  result <- data.frame(hospital=c(),state_dt=c())
  
  #test Outcome input
  test_oc<-match(outcome,c("heart attack","heart failure","pneumonia"))
  if (is.na(test_oc[1])){
    stop("Invalid outcome")
  }
  
  ## Return hospital name in that state with the given rank
  df<-data.frame(rank_hosp)
  for (st_search in st_unique){
    if (outcome=="heart attack"){
        ha<-df %>% 
        select(State,Hospital.Name,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack) %>%
        filter(State==st_search,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack!="Not Available") %>%
        arrange(Hospital.Name) %>%
        arrange(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
    }
    
    if (outcome=="heart failure"){
        ha<-df %>% 
        select(State,Hospital.Name,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure) %>%
        filter(State==st_search,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure!="Not Available") %>%
        arrange(Hospital.Name) %>%
        arrange(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
    }
    if (outcome=="pneumonia"){
        ha<-df %>% 
        select(State,Hospital.Name,Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia) %>%
        filter(State==st_search,Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia!="Not Available") %>%
        arrange(Hospital.Name) %>%
        arrange(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    }
    if (num=="best"){
      result<-rbind(result,c(ha[1,2],st_search))
    } else if (num=="worst") {
      result<-rbind(result,c(tail(ha[,2],1),st_search))
    } else {
      result<- rbind(result,c(ha[num,2],st_search))
    }
  }
  return(result)
}
