#----------------------------------------------------------------------------#

#' Calculate the pseudo-R2 value for a logistic model.
#'
#' @export
#' @param model Logistic regression model [model].
#' @return
#' @examples


pseudo_r2 <- function(model) {

 #mcfadden
 temp <- 1-(model$fit$deviance/model$fit$null.deviance)

 return(temp)

}

#----------------------------------------------------------------------------#
