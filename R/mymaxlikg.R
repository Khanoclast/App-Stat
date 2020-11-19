#' Maximum Likelihood Solution for a distribution via log-likelihood
#'
#' You have 2 inputs.
#' 'lfun' is the log likelihood equation that the function will use to solve for its max value.
#' 'theta' is the function parameter which will typically be an array in the form of 'seq(a, b, length = c)' where you guess the interval (a, b) that contains the maximum value, and 'c' is how fine of a resolution it will step through in checking for maximum values.
#' The output is a plot of the original function (before taking the log of it) with a vertical line overlaid where it finds the maximum value of this curve.
#' Additionally, the input (which is a value of theta) that results in the maximum value is returned.
#'
#' @param lfun String, theta Numeric.
#'
#' @return Numerical value of theta where the maximum point of the distribution is located.
#'
#' @examples
#' mymaxlikg(lfun = "logbin2", theta = seq(0, 1, length = 10000))
#'
#' @export
mymaxlikg = function(lfun = "logbin2", theta) { # default log lik is a combination bin
  nth = length(theta)  # nu. of values used in theta
  thmat = matrix(theta, nr = nth, nc = 1, byrow = TRUE) # Matrix of theta
  z = apply(thmat, 1, lfun) # z holds the log lik values
  zmax = max(which(z == max(z)))  # finding the INDEX of the max lik
  plot(theta, exp(z), type = "l") # plot of lik
  abline(v = theta[zmax], col = "Blue")   #  verical line through max
  axis(3, theta[zmax], round(theta[zmax], 4))  # one tick on the third axis
  theta[zmax]   # theta corresponding to max lik
}
