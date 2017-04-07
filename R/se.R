#----------------------------------------------------------------------------#

#' Generate standard errors.
#'
#' @export
#' @param x Vector of numbers [numeric].
#' @return
#' @examples


se <- function(x) {
  sd(x,na.rm=T)/sqrt(length(x[!is.na(x)]))
}

#----------------------------------------------------------------------------#

