#----------------------------------------------------------------------------#

#' Calculate a percentage. 
#' @export
#' @param num Numerator [numeric]
#' @param denom Denominator [numeric]
#' @param digit Number of digits to round to [integer]
#' @return Percentage [numeric]
#' @examples
#' TBC

perc <- function(num, denom, digit=1) {

  round((num/denom)*100, digit)

}

#----------------------------------------------------------------------------#
