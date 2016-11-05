best <- function(state, outcome) {
      ## Read outcome data
      df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
     
      ## Check that state is valid
      if(!is.element(state, state.abb)){
            stop("invalid state")
      }
      
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
      
      ## Return hospital name in that state with lowest 30-day death
      ## rate
      
      ## coerce outcome column to numeric
      df[, col] <- suppressWarnings(as.numeric(df[, col]))
      
      ## subset state, and remove NA's
      oc <- df[!is.na(df[, col]) & df[,7] == state, c(2, col)]
      
      ## return first result
      head(oc[order(oc[,2],oc[,1]),1],1)
}