source("R/private_checker_functions.R")
source("R/private_auxiliary_functions.R")

#' @title bin_choose
#' @description calculate the number of combinations in which k successes can occur in n trials.
#' @param n number of trials
#' @param k number of successes
#' @return a numerical value of the number of combinations if inputs are valid
#' @export
#' @example
#' bin_choose(n = 5, k = 3)
#' bin_choose(n = 10, k = 1:6)
#' bin_choose(8, 3:5)
bin_choose <- function(n, k){
  if (any(k>n)){
    stop("k cannot be greater than n")
  }
  else{
    res <- factorial(n)/(factorial(k)*factorial(n-k))
    return(res)
  }
}


#' @title bin_probability
#' @description calculates the probability in which k successes can occur in n trials.
#' @param success number of successes
#' @param trials total number of trials
#' @param prob probability of success in each trial
#' @return a numerical value of the probability in which k successes can occur in n trials.
#' @export
#' @example
#' bin_probability(3:5, 10, 0.3)
#' bin_probability(success = 6, trials = 9, prob = 0.7)
bin_probability <- function(success, trials, prob){
  if(check_trials(trials)!=TRUE){
    stop('invalid trials value')
  }
  else if(check_prob(prob)!=TRUE){
    stop('invalid probability value')
  }
  else if(check_success(success, trials)!=TRUE){
    stop('invalid success value')
  }
  else{
    return(bin_choose(trials, success) * prob^success * (1-prob)^(trials-success))
  }
}

#' @title bin_distribution
#' @description constructs a table of successes and probability.
#' @param trials total number of trials
#' @param prob probability of success in each trial
#' @return an object of both 'bindis' and 'data.frame' that displays the success and its corresponding probability
#' @export
#' @example
#' bin_distribution(100, 0.48)
#' bin_distribution(5, 0.3)
bin_distribution <- function(trials, prob){
  x <- data.frame(success = 0:trials, probability = bin_probability(0:trials, trials, prob))
  class(x) <- c('bindis', 'data.frame')
  return(x)
}

#' @export a bar plot of successes and its probability
plot.bindis <- function(x){
  barplot(x$probability, names.arg = x$success, xlab = 'successes', ylab = 'probability')
}

#' @title bin_cumulative
#' @description constructs a table of successes, probability, and cumulative probability.
#' @param trials total number of trials
#' @param prob probability of success in each trial
#' @return an object of both 'bincum' and 'data.frame' that has columns of success, probability, and cumulative probability
#' @export
#' @example
#' bin_cumulative(trials = 100, prob = 0.53)
#' bin_cumulative(5, 0.4)
bin_cumulative <- function(trials, prob){
  x <- bin_distribution(trials, prob)
  x$cumulative = cumsum(x$probability)
  class(x) <- c('bincum', 'data.frame')
  return(x)
}

#' @export a line and point plot of successes and its cumulative probability
plot.bincum <- function(data){
  plot(x=data$success, y=data$cumulative, type = 'o', xlab = 'successes', ylab = 'probability')
}


#' @title bin_variable
#' @description creates the binomial random variable object
#' @param trials total number of trials
#' @param prob probability of success in each trial
#' @return an object of class 'binvar'
#' @export
#' @example
#' bin_variable(50, 0.3)
#' bin_variable(trials = 100, prob = 0.36)
bin_variable <- function(trials, prob){
  if(check_trials(trials)!=TRUE){
    stop('invalid trials value')
  }
  else if(check_prob(prob)!=TRUE){
    stop('invalid probability value')
  }
  else{
    x <- list(trials = trials, prob = prob)
    class(x) <- c('binvar')
    return(x)
  }
}

#' @export summary summary values
print.binvar <- function(x){
  cat("\"Binomial variable\"")
  cat("\n")
  cat("\nParameters")
  cat("\n- number of trials: ", x$trials)
  cat("\n- prob of success :", x$prob)
}


#' @export
summary.binvar <- function(x){
  y <- list(trials=x$trials, prob=x$prob, mean=aux_mean(x$trials, x$prob),
       var=aux_variance(x$trials, x$prob), mode=aux_mode(x$trials, x$prob),
       skew=aux_skewness(x$trials, x$prob), kurt=aux_kurtosis(x$trials, x$prob))
  class(y) <- c('summary.binvar')
  return(y)
}

#' @export summary summary values
print.summary.binvar <- function(x){
  cat("\"Summary Binomial\"")
  cat("\n")
  cat("\nParameters")
  cat("\n- number of trials: ", x$trials)
  cat("\n- prob of success :", x$prob)
  cat("\n")
  cat("\nMeasures")
  cat("\n- mean    : ", x$mean)
  cat("\n- variance: ", x$var)
  cat("\n- mode    : ", x$mode)
  cat("\n- skewness: ", x$skew)
  cat("\n- kurtosis: ", x$kurt)
}

#' @title bin_mean
#' @description calculates the mean of the binomial distribution
#' @param trials total number of trials
#' @param prob probability of success
#' @return the mean of the binomial distribution
#' @export
#' @example
#' bin_mean(10, 0.5)
#' bin_mean(trials = 5, prob = 0.6)
bin_mean <- function(trials, prob){
  if(check_trials(trials)!=TRUE){
    stop('invalid trials value')
  }
  else if(check_prob(prob)!=TRUE){
    stop('invalid probability value')
  }
  else{
    return(aux_mean(trials, prob))
  }
}

#' @title bin_variance
#' @description calculates the variance of the binomial distribution
#' @param trials total number of trials
#' @param prob probability of success
#' @return the variance of the binomial distribution
#' @export
#' @example
#' bin_variance(10, 0.5)
#' bin_variance(trials = 5, prob = 0.6)
bin_variance <- function(trials, prob){
  if(check_trials(trials)!=TRUE){
    stop('invalid trials value')
  }
  else if(check_prob(prob)!=TRUE){
    stop('invalid probability value')
  }
  else{
    return(aux_variance(trials, prob))
  }
}

#' @title bin_mode
#' @description calculates the mode of the binomial distribution
#' @param trials total number of trials
#' @param prob probability of success
#' @return the mode of the binomial distribution
#' @export
#' @example
#' bin_mode(10, 0.5)
#' bin_mode(trials = 5, prob = 0.6)
bin_mode <- function(trials, prob){
  if(check_trials(trials)!=TRUE){
    stop('invalid trials value')
  }
  else if(check_prob(prob)!=TRUE){
    stop('invalid probability value')
  }
  else{
    return(aux_mode(trials, prob))
  }
}

#' @title bin_skewness
#' @description calculates the skewness of the binomial distribution
#' @param trials total number of trials
#' @param prob probability of success
#' @return the skewness of the binomial distribution
#' @export
#' @example
#' bin_skewness(10, 0.5)
#' bin_skewness(trials = 5, prob = 0.6)
bin_skewness <- function(trials, prob){
  if(check_trials(trials)!=TRUE){
    stop('invalid trials value')
  }
  else if(check_prob(prob)!=TRUE){
    stop('invalid probability value')
  }
  else{
    return(aux_skewness(trials, prob))
  }
}

#' @title bin_kurtosis
#' @description calculates the kurtosis of the binomial distribution
#' @param trials total number of trials
#' @param prob probability of success
#' @return the kurtosis of the binomial distribution
#' @export
#' @example
#' bin_kurtosis(10, 0.5)
#' bin_kurtosis(trials = 5, prob = 0.6)
bin_kurtosis <- function(trials, prob){
  if(check_trials(trials)!=TRUE){
    stop('invalid trials value')
  }
  else if(check_prob(prob)!=TRUE){
    stop('invalid probability value')
  }
  else{
    return(aux_kurtosis(trials, prob))
  }
}
