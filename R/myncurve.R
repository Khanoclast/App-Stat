#' Normal distribution curve with lower-tail shading and calculation
#'
#' You have 3 inputs.
#' 'q' is the quantile you're interested in finding the probability from (-infinity, q). This is also the horizontal location on the curve that the shaded area under the curve from x=-infinity to x=q.
#' 'mu' is the mean of the curve (the horizontal location for the mid point of the symetric curve).
#' 'sigma' is the standard deviation of the curve (how narrow or wide the curve behaves).
#' The output is the lower-tail probability that a random specimen's result will occur at less than or equal to 'q'.
#'
#' @param q Numeric, mu Numeric, sigma Numeric.
#'
#' @return Graph of norm(mean = mu, sd = sigma), a red shaded polygon (representing a lower-tail probability) overlaid on the graph, and a list returning the qnorm(q, mu, sigma) value.
#'
#' @examples
#' mynbin(10, 10, 4)
#'
#' @export
myncurve = function(q, mu, sigma){
  curve(dnorm(x, mean = mu, sd = sigma), xlim = c(mu - 4 * sigma, mu + 4 * sigma))
  xcurve = seq(mu - 4 * sigma, q, length = 1000)
  ycurve = dnorm(xcurve, mean = mu, sd = sigma)
  polygon(c(mu - 4 * sigma, xcurve, q), c(0, ycurve, 0), col = "Red")
  prob = pnorm(q, mean = mu, sd = sigma)
  prob = round(prob, 4)
  return(list(prob))
}
