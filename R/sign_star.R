#----------------------------------------------------------------------------#

#' Determine the significance (star format) of a p-value. 
#' @export
#' @param  TBC
#' @return TBC
#' @examples
#' TBC

sign_star <- function(p_value) {

  # sign_temp <- ifelse(p_value < .001, "**** ", 
  #               ifelse(p_value< .01, "***  ", 
  #               ifelse(p_value < .05, "**   ",
  #               ifelse(p_value < .1, "*   ",
  #               "    "))))


  ## *: Sign at 10%, **: Sign at 5%, *: Sign at 1%
  sign_temp <- ifelse(p_value< .01, "***  ", 
                 ifelse(p_value < .05, "**   ",
                 ifelse(p_value < .1, "*   ",
                  "    ")))

  return(sign_temp)

}

#----------------------------------------------------------------------------#
