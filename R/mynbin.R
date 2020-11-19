#' Negative Binomial Probability at a Point
#'
#' You have 3 inputs.
#' 'p' is the probability that a single Bernoulli trial is successful.
#' 'r' is how many successful trials you are looking for.
#' 'y' is the number trials.
#' The output is the probability of 'r' number of successes occurring given 'y' number of trials.
#'
#' @param y Numeric, r Numeric, p Numeric.
#'
#' @return Numeric.
#'
#' @examples
#' mynbin(y = 6, r = 4, p = 0.8)
#'
#' @export
mynbin=function(y,r,p){
  choose(y-1,r-1)*p^r*(1-p)^(y-r)
}
