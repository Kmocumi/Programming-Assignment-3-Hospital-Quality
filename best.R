# Create the function
best<-function(state, outcome){
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", header=TRUE)
  # Create dataframe with useful columns
  df   <- as.data.frame(cbind(data[, 2],   # hospital
                              data[, 7],   # state
                              data[, 11],  # heart attack
                              data[, 17],  # heart failure
                              data[, 23]), # pneumonia
                        stringsAsFactors = FALSE)
  # Name the columns of the dataframe df
  colnames(df) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  if(!state %in% df[, "state"]){
    stop('invalid state')
  } else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  } else {
    si <- which(df[, "state"] == state)
    ts <- df[si, ]    # extracting data for the called state
    oi <- as.numeric(ts[, eval(outcome)])
    min_val <- min(oi, na.rm = TRUE)
    result  <- ts[, "hospital"][which(oi == min_val)]
    output  <- result[order(result)]
  }
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  return(output)
}