#private function that calculates the mean value given the input number of trials and probability
aux_mean <- function(trials, prob){
  return(trials * prob)
}

#private function that calculates the variance given the input number of trials and probability
aux_variance <- function(trials, prob){
  return(trials * prob * (1-prob))
}

#private function that calculates the mode given the input number of trials and probability
aux_mode <- function(trials, prob){
  mode <- as.integer(trials * prob + prob)
  return(mode)
}

#private function that calculates the skewness given the input number of trials and probability
aux_skewness <- function(trials, prob){
  skew <- (1-2*prob)/sqrt(trials*prob*(1-prob))
  return(skew)
}

#private function that calculates the kurtosis given the input number of trials and probability
aux_kurtosis <- function(trials, prob){
  kurt <- (1-6*prob*(1-prob))/(trials*prob*(1-prob))
  return(kurt)
}
