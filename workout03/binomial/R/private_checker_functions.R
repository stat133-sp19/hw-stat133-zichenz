#private function that test if an input prob is a valid probability value
check_prob <- function(prob){
  if (0>prob & prob>1){
    stop("\n'prob' has to be a number between 0 and 1")
  }
  else{
    return(TRUE)
  }
}

#private function that test if an input trials is a valid value for number of trials
check_trials <- function(trials){
  if(trials<0 | is.integer(trials) == TRUE){
    stop("\n'trials' has to be a non-negative integer")
  }
  else{
    return(TRUE)
  }
}

#private function that test if an input success is a valied value for number os successes
check_success <- function(success, trials){
  if(any(success>trials)){
    stop("'success' cannot be greater than 'trials'")
  }
  else if(any(success<0)){
    stop('invalied success value')
  }
  else{
    return(TRUE)
  }
}
