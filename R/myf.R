#' Creates a piecewise function of two lines connected at x=18 in order to make a continuous curve.
#'
#' You feed the function an x-value and a vector of 3 numbers to be the 3 coefficients, and it'll calculate a value and return that value.
#' If x less than or equal to 18, then the function will return coef[1]+coef[2]*x.
#' If x > 18, then the function will return coef[1]+coef[2]*x+coef[3]*(x-18)
#'
#' @param x Numeric, coef Numeric Vector (of at least 3 elements).
#'
#' @return Numeric.
#'
#' @examples
#' beta = c(1,2,3)
#' myf(0, beta)
#'
#' @export
myf = function(x,coef){
  coef[1]+coef[2]*(x) + coef[3]*(x-18)*(x-18>0)
}
