# create a rankhospital function
rankhospital <- function(state, outcome, num = "best"){
  ## Read outcome data
  dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character", header=TRUE)
  df   <- as.data.frame(cbind(dat[, 2],   # hospital
                              dat[, 7],   # state
                              dat[, 11],  # heart attack
                              dat[, 17],  # heart failure
                              dat[, 23]), # pneumonia
                        stringsAsFactors = FALSE)
  # Name the columns of the dataframe df
  colnames(df) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  ## Check that state and outcome are valid
  if (!state %in% df[, "state"]) {
    stop('invalid state')
  } else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  } else if (is.numeric(num)) {
    si <- which(df[, "state"] == state)
    ts <- df[si, ]                     # extracting dataframe for the called state
    ts[, eval(outcome)] <- as.numeric(ts[, eval(outcome)])
    ts <- ts[order(ts[, eval(outcome)], ts[, "hospital"]), ]
    output <- ts[, "hospital"][num]
  } else if (!is.numeric(num)){
    if (num == "best") {
      output <- best(state, outcome)
    } else if (num == "worst") {
      si <- which(fd[, "state"] == state)
      ts <- fd[si, ]    
      ts[, eval(outcome)] <- as.numeric(ts[, eval(outcome)])
      ts <- ts[order(ts[, eval(outcome)], ts[, "hospital"], decreasing = TRUE),]
      output <- ts[, "hospital"][1]
    } else {
      stop('invalid rank')
    }
  }
  return(output)
}
  
