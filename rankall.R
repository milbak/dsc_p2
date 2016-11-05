rankall <- function(outcome, num = "best") {
      # Read the csv into a data frame
      df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      
      ## Set outcome column based or stop if invalid
      col <- NULL
      if(outcome == "heart attack"){
            col <- 11
      } else if(outcome == "heart failure") {
            col <- 17
      } else if(outcome == "pneumonia") {
            col <- 23
      } else {
            stop("invalid outcome")
      }
      
      ## Coerce outcome column to numeric
      df[, col] <- suppressWarnings(as.numeric(df[, col]))
      
      ## Subset, hostpital(2), state(7), and outcome(col)
      oc <- df[, c(2, 7, col)]
      
      ## Split by state as a factor
      oc <- split(oc,as.factor(oc$State))
      
      ## Find the hospitals by state 
      result <- sapply(oc, function(oc_state){
                              ## Remove NA's for outcome
                              oc_state <- oc_state[!is.na(oc_state[,3]),]
                              
                              ## Check if num is numeric and less than
                              ## the number of rows in the subset
                              if(class(num) == "numeric" & 
                                 num > nrow(oc_state)){
                                    hos <- NA
                              } else {
                                    ## Order by outcome and hospital
                                    oc_state <- oc_state[order(oc_state[,3], 
                                                               oc_state[,1]),]
                                    
                                    ## Find hospital by num
                                    if(num == "best") {
                                          hos <- head(oc_state,1)[,1]
                                    } else if(num == "worst"){
                                          hos <-tail(oc_state,1)[,1]
                                    } else if(num <= nrow(oc_state)) {
                                          hos <- tail(head(oc_state, 
                                                           num), 1)[,1]
                                    } 
                              }
                              
                              ## Return the hostpital
                              return(hos)
                        })
      ## Create data frame
      data.frame(hospital = result, state = names(result))
}