#' Creates Samples from a Uniform Distribution, Calc's the Mean Statistic w/ Histograph
#'
#' You have 4 inputs.
#' 'n' is the size of each sample
#' 'iter' is the number of samples you want generated
#' 'a' is the lowerbound of the uniform distribution (that the samples will be pulled from)
#' 'b' is the upperbound of the uniform distribution (that the samples will be pulled from)
#' The output is a histograph of the means of each sample (so there are 'iter' many datapoints for the graph).
#' Additionally, 3 curves are overlaid on the graph:
#' - a density curve made from the sample distribution
#' - a theoretical normal curve
#' - the density from which the samples were taken
#'
#' @param n Numeric, iter Numeric, a Numeric, b Numeric.
#'
#' @return Histograph of the means of each sample (with 3 additional curves overlaid)
#'
#' @examples
#' mycltu(30, 10000, 0, 10)
#'
#' @export
mycltu = function(n, iter, a = 0, b = 10){
  y = runif(n * iter, a, b)
  data = matrix(y, nr = n, nc = iter, byrow = TRUE)
  w = apply(data, 2, mean)
  param = hist(w, plot = FALSE)
  ymax = max(param$density)
  ymax = 1.1 * ymax
  hist(w, freq = FALSE, ylim = c(0, ymax), main = paste("Histogram of sample mean", "\n", "sample size= ", n, sep = ""), xlab = "Sample mean")
  lines(density(w), col = "Blue", lwd = 3)
  curve(dnorm(x, mean = (a + b) / 2, sd = (b - a) / (sqrt(12 * n))), add = TRUE, col = "Red", lty = 2, lwd = 3)
  curve(dunif(x, a, b), add = TRUE, lwd = 4)
}
