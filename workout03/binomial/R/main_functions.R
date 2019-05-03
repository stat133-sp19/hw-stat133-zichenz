source("R/private_checker_functions.R")
source("R/private_auxiliary_functions.R")

#' @title
#' @description
#' @param n
#' @param k
#' @return
#' @export
#' @example
bin_choose <- function(n, k){
  if (any(k>n)){
    stop("k cannot be greater than n")
  }
  else{
    res <- factorial(n)/(factorial(k)*factorial(n-k))
    return(res)
  }
}


#' @title
#' @description
#' @param success
#' @param trials
#' @param prob
#' @return
#' @export
#' @example
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

#' @title
#' @description
#' @param trials
#' @param prob
#' @return
#' @export
#' @example
bin_distribution <- function(trials, prob){
  x <- data.frame(success = 0:trials, probability = bin_probability(0:trials, trials, prob))
  class(x) <- c('bindis', 'data.frame')
  return(x)
}

#' @export
plot.bindis <- function(x){
  barplot(x$probability, names.arg = x$success, xlab = 'successes', ylab = 'probability')
}

#' @title
#' @description
#' @param trials
#' @param prob
#' @return
#' @export
#' @example
bin_cumulative <- function(trials, prob){
  x <- bin_distribution(trials, prob)
  x$cumulative = cumsum(x$probability)
  class(x) <- c('bincum', 'data.frame')
  return(x)
}

#' @export
plot.bincum <- function(data){
  plot(x=data$success, y=data$cumulative, type = 'o', xlab = 'successes', ylab = 'probability')
}


#' @title
#' @description
#' @param trials
#' @param prob
#' @return an object of class 'binvar'
#' @export
#' @example
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

#' @export
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

#' @export
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

#' @title
#' @description
#' @param trials
#' @param prob
#' @return
#' @export
#' @example
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

#' @title
#' @description
#' @param trials
#' @param prob
#' @return
#' @export
#' @example
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

#' @title
#' @description
#' @param trials
#' @param prob
#' @return
#' @export
#' @example
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

#' @title
#' @description
#' @param trials
#' @param prob
#' @return
#' @export
#' @example
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

#' @title
#' @description
#' @param trials
#' @param prob
#' @return
#' @export
#' @example
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
