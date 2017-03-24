#----------------------------------------------------------------------------#

#' Convert a character vector into a formula object.
#' @export
#' @param  TBC
#' @return TBC
#' @examples
#' TBC

form <- function(var_list, formula=TRUE) {

  temp_var <- paste0(var_list, collapse=" + ")

  if (formula==TRUE) {

    temp_formula <- as.formula(paste0("~ ", temp_var))
    return(temp_formula)

  } else {

    return(temp_var)
  }

}

#----------------------------------------------------------------------------#
