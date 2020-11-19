#' 95% Confidence Interval for mean of 1 normally distributed, random sample
#'
#' You have 1 input.
#' 'x' is your data. It should be an array of numbers corresponding to some normally distributed, random sample.
#' The output is an array of 2 values. The first value is the lower bound of the 95% confidence interval.
#' And the second value is the upper bound of the 95% confidence interval.
#'
#' @param x Numeric.
#'
#' @return Array(lower bound of ci, upper bound of ci)
#'
#' @examples
#' myci(rnorm(25, mean = 10, sd = 5))
#'
#' @export
myci = function(x) {
  L = mean(x) - qt(0.975, df = length(x) - 1) * sd(x) / sqrt(length(x))
  U = mean(x) + qt(0.975, df = length(x) - 1) * sd(x) / sqrt(length(x))
  c(L, U)
}
