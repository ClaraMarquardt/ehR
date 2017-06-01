#----------------------------------------------------------------------------#

#' Determine the significance (star format) of a p-value. 
#' @export
#' @param  TBC
#' @return TBC
#' @examples
#' TBC

sign_star <- function(p_value) {

  p_value <- as.numeric(gsub("<|>", "", p_value))

  ## *: Sign at 10%, **: Sign at 5%, *: Sign at 1%
  if (!is.na(p_value[1])) {

      sign_temp <- ifelse(p_value < 0.01, "***", ifelse(p_value <
          0.05, "**", ifelse(p_value < 0.1, "*", "    ")))
      
      return(sign_temp)

    } else {

      return("    ")
    }

}

#----------------------------------------------------------------------------#

